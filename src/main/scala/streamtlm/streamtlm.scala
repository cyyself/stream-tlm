package streamtlm

import chisel3._
import chisel3.util._
import java.nio.file._
import streamtlm.adaptor._
import streamtlm.bus._

case class EdgeInterface (
    val name: String = "Edge"
)

class AXI4EdgeParams(
    override val name: String = "AXI4",
    val axisAddr: Int,
    val interface: AXI4Params = new AXI4Params(),
    val isMaster: Boolean = true
) extends EdgeInterface(name)

class IntBusEdgeParams(
    override val name: String = "INT",
    val axisAddr: Int,
    val width: Int = 1
) extends EdgeInterface(name)

class StreamTLMParams(
    val axisWidth: Int = 512,
    val axisAddrBits: Int = 6, // supports up to 64 AXI4/INT edges
    val edges: Seq[EdgeInterface] = Seq(),
    val outDir: Path
)

class StreamTLMTop(params: StreamTLMParams) extends Module {
    /* Params used to generate Wrapper { */
    var ioList: Seq[(Boolean, Int, String)] = Seq() // (isOutput, width, name)
    var instConnect: Seq[(String, String)] = Seq() // (instPort, topPort)
    /* Params used to generate Wrapper } */
    val axis = IO(new Bundle {
        val in  = Flipped(Decoupled(new AXIS(new AXISParams(dataWidth = params.axisWidth))))
        val out =         Decoupled(new AXIS(new AXISParams(dataWidth = params.axisWidth)))
        ioList = ioList ++ in.bits.genIO("AXIS_IN", false)
        ioList = ioList ++ out.bits.genIO("AXIS_OUT", true)
        instConnect = instConnect ++ in.bits.genInstConnect("AXIS_IN", "axis_in")
        instConnect = instConnect ++ out.bits.genInstConnect("AXIS_OUT", "axis_out")
    })

    var axisPorts = scala.collection.mutable.Map[Int, (DecoupledIO[AXIS], DecoupledIO[AXIS])]() // axisAddr -> (in, out)
    params.edges.zipWithIndex.foreach { case (edge, idx) =>
        edge match {
            case axi4: AXI4EdgeParams => {
                val axi = new AXI4(axi4.interface)
                val exportIO = if (axi4.isMaster) IO(Flipped(axi)) else IO(axi)
                exportIO.suggestName(axi4.name)
                ioList = ioList ++ axi.genIO(axi4.name, axi4.isMaster)
                instConnect = instConnect ++ axi.genInstConnect(axi4.name, axi4.name)
                val adaptor = Module(new AXI4Adaptor(
                    axiParams = axi4,
                    axisParams = new AXISParams(dataWidth = params.axisWidth),
                    axisAddr = idx,
                    addrBits = params.axisAddrBits
                ))
                axisPorts(idx) = (adaptor.io.axis_in, adaptor.io.axis_out)
                adaptor.io.axi <> exportIO
            }
            case int: IntBusEdgeParams => {
                val intbus = new IntBus(new IntBusParams(width = int.width))
                val exportIO = IO(Output(intbus))
                exportIO.suggestName(int.name)
                ioList = ioList ++ intbus.genIO(int.name, true)
                instConnect = instConnect ++ intbus.genInstConnect(int.name, int.name)
                val adaptor = Module(new InterruptAdaptor(
                    intParams = int,
                    axisParams = new AXISParams(dataWidth = params.axisWidth),
                    axisAddr = idx,
                    addrBits = params.axisAddrBits
                ))
                axisPorts(idx) = (adaptor.io.axis_in, adaptor.io.axis_out)
                exportIO := adaptor.io.int
            }
            case _ => println(s"Unknown edge type: ${edge.name}")
        }
    }

    // tx
    val txArb = Module(new RRArbiter(new AXIS(new AXISParams(dataWidth = params.axisWidth)), axisPorts.size))
    axisPorts.toSeq.zipWithIndex.foreach { case ((addr, (in, out)), idx) =>
        out <> txArb.io.in(idx)
        when (out.fire) {
            assert(out.bits.last.get, "TX AXIS packet must fit in a single beat")
        }
    }
    axis.out <> txArb.io.out

    // rx
    val rxQueue = Module(new Queue(new AXIS(new AXISParams(dataWidth = params.axisWidth)), 2))
    axis.in <> rxQueue.io.enq
    val curRxDst = rxQueue.io.deq.bits.data(params.axisAddrBits, 0).asUInt
    rxQueue.io.deq.ready := false.B
    axisPorts.toSeq.zipWithIndex.foreach { case ((addr, (in, out)), idx) =>
        in.bits := rxQueue.io.deq.bits
        when (curRxDst === idx.U) {
            in.valid := rxQueue.io.deq.valid
            rxQueue.io.deq.ready := in.ready
        } .otherwise {
            in.valid := false.B
        }
    }

    when (axis.in.fire) {
        assert(axis.in.bits.last.get, "TX AXIS packet must fit in a single beat")
    }

    def genVerilogWrapper(): Unit = {
        val filePath = params.outDir.resolve("streamtlm_top.v")
        Files.createDirectories(filePath.getParent())
        Files.write(filePath, s"""
module streamtlm_wrapper (
    input clock,
    input reset,
${
    ioList.map { case (isOutput, width, name) =>
        s"    ${if (isOutput) "output" else "input"} [${width-1}:0] ${name},"
    }.mkString("\n").stripSuffix(",")
}
);

    StreamTLMTop streamtlm_inst (
        .clock(clock),
        .reset(reset),
${
    instConnect.map { case (instPort, topPort) =>
        s"        .${instPort}(${topPort}),"
    }.mkString("\n").stripSuffix(",")
}
    );

endmodule
    """.strip().getBytes())
    } /* End of genVerilogWrapper */
    genVerilogWrapper()
}
