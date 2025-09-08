package streamtlm.bus

import chisel3._
import chisel3.util._

class IntBusParams(
    val width: Int = 1
)

class IntBus(params: IntBusParams) extends Bundle {
    val vec = UInt(params.width.W)

    def genIO(prefix: String, direction: Boolean): Seq[(Boolean, Int, String)] = {
        Seq((direction, params.width, prefix))
    }

    def genInstConnect(prefix: String, dstName: String): Seq[(String, String)] = {
        Seq((f"${dstName}_vec", prefix))
    }
}
