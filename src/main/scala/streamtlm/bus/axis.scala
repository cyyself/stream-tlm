package streamtlm.bus

import chisel3._
import chisel3.util._

class AXISParams (
    val dataWidth: Int = 32,
    val userWidth: Int = 0,
    val idWidth: Int = 0,
    val destWidth: Int = 0,
    val hasLast: Boolean = true,
    val hasKeep: Boolean = true,
    val hasStrb: Boolean = false
)

class AXIS(params: AXISParams) extends Bundle {
    val data = UInt(params.dataWidth.W)
    val user = if (params.userWidth > 0) Some(UInt(params.userWidth.W)) else None
    val id = if (params.idWidth > 0) Some(UInt(params.idWidth.W)) else None
    val dest = if (params.destWidth > 0) Some(UInt(params.destWidth.W)) else None
    val last = if (params.hasLast) Some(Bool()) else None
    val keep = if (params.hasKeep) Some(UInt((params.dataWidth/8).W)) else None
    val strb = if (params.hasStrb) Some(UInt((params.dataWidth/8).W)) else None

    def genIO(prefix: String, direction: Boolean): Seq[(Boolean, Int, String)] = {
        val res: Seq[(Boolean, Int, String)] = Seq(
            (direction, params.dataWidth, s"${prefix}_TDATA"),
            (if (params.userWidth > 0) direction else false, if (params.userWidth > 0) params.userWidth else 0, s"${prefix}_TUSER"),
            (if (params.idWidth > 0) direction else false, if (params.idWidth > 0) params.idWidth else 0, s"${prefix}_TID"),
            (if (params.destWidth > 0) direction else false, if (params.destWidth > 0) params.destWidth else 0, s"${prefix}_TDEST"),
            (if (params.hasLast) direction else false, if (params.hasLast) 1 else 0, s"${prefix}_TLAST"),
            (if (params.hasKeep) direction else false, if (params.hasKeep) params.dataWidth/8 else 0, s"${prefix}_TKEEP"),
            (if (params.hasStrb) direction else false, if (params.hasStrb) params.dataWidth/8 else 0, s"${prefix}_TSTRB"),
            (direction, 1, s"${prefix}_TVALID"),
            (!direction, 1, s"${prefix}_TREADY")
        ).filter(_._2 > 0)
        res
    }

    def genInstConnect(prefix: String, dstName: String): Seq[(String, String)] = {
        val validNames = Set(genIO(prefix, true).map(_._3):_*)
        val res: Seq[(String, String)] = Seq(
            (s"${dstName}_bits_data", s"${prefix}_TDATA"),
            (s"${dstName}_bits_user", s"${prefix}_TUSER"),
            (s"${dstName}_bits_id", s"${prefix}_TID"),
            (s"${dstName}_bits_dest", s"${prefix}_TDEST"),
            (s"${dstName}_bits_last", s"${prefix}_TLAST"),
            (s"${dstName}_bits_keep", s"${prefix}_TKEEP"),
            (s"${dstName}_bits_strb", s"${prefix}_TSTRB"),
            (s"${dstName}_valid", s"${prefix}_TVALID"),
            (s"${dstName}_ready", s"${prefix}_TREADY")
        ).filter(x => validNames.contains(x._2))
        res
    }
}