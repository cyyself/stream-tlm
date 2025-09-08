package streamtlm.adaptor

import chisel3._
import chisel3.util._
import streamtlm._
import streamtlm.bus._

class AXI4Adaptor(axiParams: AXI4EdgeParams,
                  axisParams: AXISParams,
                  axisAddr: Int,
                  addrBits: Int) extends Module {
    val io = IO(new Bundle {
        val axi = if (axiParams.isMaster) Flipped(new AXI4(axiParams.interface)) else new AXI4(axiParams.interface)
        val axis_in  = Flipped(Decoupled(new AXIS(axisParams)))
        val axis_out =         Decoupled(new AXIS(axisParams))
    })

    val txChannels = if (axiParams.isMaster) Seq(io.axi.b, io.axi.r) else Seq(io.axi.ar, io.axi.aw, io.axi.w)
    val rxChannels = if (axiParams.isMaster) Seq(io.axi.ar, io.axi.aw, io.axi.w) else Seq(io.axi.b, io.axi.r)

    /* Packet format:
        |<- headerSize ->|<- 2 ->|<- max(AR, AW, W, B, R) ->|
     */
    val headerSize = addrBits + 2 // addr + channel ID
    val maxTxFrameSize = headerSize + Seq(txChannels.map(_.bits.getWidth):_*).max
    val maxRxFrameSize = headerSize + Seq(rxChannels.map(_.bits.getWidth):_*).max
    require(maxTxFrameSize <= axisParams.dataWidth, s"AXIS dataWidth (${axisParams.dataWidth}) is too small for AXI4 tx frame (required: ${maxTxFrameSize})")

    val txArb = Module(new RRArbiter(new AXIS(axisParams), txChannels.length))
    
    txChannels.zipWithIndex.foreach { case (ch, idx) =>
        val chSize = ch.bits.getWidth
        val header = Cat(idx.U(2.W), axisAddr.U(addrBits.W))
        val frame = Cat(0.U((axisParams.dataWidth - headerSize - chSize).W), ch.bits.asUInt, header)
        ch.ready := txArb.io.in(idx).ready
        txArb.io.in(idx).valid := ch.valid
        txArb.io.in(idx).bits.data := frame
        txArb.io.in(idx).bits.last.get := true.B
        txArb.io.in(idx).bits.keep.get := Fill(((headerSize + chSize + 7)/8), 1.U(1.W))
    }
    txArb.io.out <> io.axis_out

    val rxQueue = Module(new Queue(UInt(maxRxFrameSize.W), 2))
    io.axis_in.ready := rxQueue.io.enq.ready
    rxQueue.io.enq.valid := io.axis_in.valid
    rxQueue.io.enq.bits := io.axis_in.bits.data

    val curRxDst = rxQueue.io.deq.bits(addrBits + 1, addrBits).asUInt
    rxQueue.io.deq.ready := false.B
    rxChannels.zipWithIndex.foreach { case (ch, idx) =>
        ch.bits := rxQueue.io.deq.bits(ch.bits.getWidth - 1 + headerSize, headerSize).asTypeOf(ch.bits)
        when (curRxDst === idx.U) {
            ch.valid := rxQueue.io.deq.valid
            rxQueue.io.deq.ready := ch.ready
        } .otherwise {
            ch.valid := false.B
        }
    }

    // assertions
    val rxAddr = rxQueue.io.deq.bits(addrBits - 1, 0)
    when (io.axis_in.fire) {
        assert(io.axis_in.bits.last.get, "TX AXIS packet must fit in a single beat")
        assert(curRxDst < rxChannels.length.U, "RX AXIS packet has invalid channel ID")
        assert(rxAddr === axisAddr.U, "RX AXIS packet has invalid address")
    }
}
