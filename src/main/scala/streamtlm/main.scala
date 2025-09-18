package streamtlm

import circt.stage.ChiselStage
import dataclass.data
import java.nio.file._
import streamtlm.bus._

object StreamTLM extends App {
    val outDir = Paths.get("build/rtl")
    val params = new StreamTLMParams(
        edges = Seq(
            new AXI4EdgeParams  (name = "MMIO", axisAddr = 0, isMaster = false, interface = new AXI4Params(addrWidth = 32, dataWidth = 64)),
            new AXI4EdgeParams  (name = "MEM",  axisAddr = 1, isMaster = false, interface = new AXI4Params(addrWidth = 32, dataWidth = 64)),
            new AXI4EdgeParams  (name = "DMA",  axisAddr = 2, isMaster = true,  interface = new AXI4Params(addrWidth = 32, dataWidth = 64, idWidth = 8)),
            new IntBusEdgeParams(name = "INT",  axisAddr = 3, width = 4)
        ),
        outDir = outDir
    )
    ChiselStage.emitSystemVerilogFile(
        new StreamTLMTop(params),
        Array(
            "-td", outDir.toString()
        ),
        firtoolOpts = Array(
            "-disable-all-randomization",
            "-default-layer-specialization=enable",
            "--split-verilog"
        )
    )
}