package streamtlm.adaptor

import chisel3._
import chisel3.util._
import streamtlm._
import streamtlm.bus._

object InterruptOp {
    val Set         = 0.U
    val Clear       = 1.U
    val AssertFor   = 2.U
    val ClearAssert = 3.U
}

class InterruptAdaptor(intParams: IntBusEdgeParams,
                       axisParams: AXISParams,
                       axisAddr: Int,
                       addrBits: Int) extends Module {
    val io = IO(new Bundle {
        val int = Output(new IntBus(new IntBusParams(width = intParams.width)))
        val axis_in  = Flipped(Decoupled(new AXIS(axisParams)))
        val axis_out =         Decoupled(new AXIS(axisParams))
    })

    io.axis_out := DontCare // No output for this adaptor

    /* Packet format:
        |<- headerSize ->|<- Type (2 bits) ->|<- Dst (8 bits) ->|<- Data (optional) ->|
        Type: 0 - set interrupt
              1 - clear interrupt
              2 - assert interrupt for given cycles (16 bits optional data)
              3 - clear asserted interrupt
     */
    require(intParams.width <= 256, "Interrupt width too large (max 256)")
    val outRegs = RegInit(VecInit(Seq.fill(intParams.width)(false.B)))
    val counters = RegInit(VecInit(Seq.fill(intParams.width)(0.U(16.W))))

    counters.zipWithIndex.foreach { case (cnt, idx) =>
        when (cnt =/= 0.U) {
            cnt := cnt - 1.U
        }
    }
    io.int.vec := Cat(outRegs.zip(counters).map { case (reg, cnt) => RegNext(reg || (cnt =/= 0.U)) }.reverse)

    val rxAddr = io.axis_in.bits.data(addrBits - 1, 0)
    val rxType = io.axis_in.bits.data(addrBits + 1, addrBits)
    val rxDst  = io.axis_in.bits.data(addrBits + 9, addrBits + 2)
    when (io.axis_in.fire) {
        assert(rxAddr === axisAddr.U, "Interrupt AXIS packet received at wrong address")
        assert(rxDst < intParams.width.U, "Interrupt AXIS packet received with invalid dst")
        switch (rxType) {
            is (InterruptOp.Set) {
                outRegs(rxDst) := true.B
            }
            is (InterruptOp.Clear) {
                outRegs(rxDst) := false.B
            }
            is (InterruptOp.AssertFor) {
                val cycles = io.axis_in.bits.data(addrBits + 25, addrBits + 10)
                counters(rxDst) := cycles
            }
            is (InterruptOp.ClearAssert) {
                counters(rxDst) := 0.U
            }
        }
    }
    io.axis_in.ready := true.B
}