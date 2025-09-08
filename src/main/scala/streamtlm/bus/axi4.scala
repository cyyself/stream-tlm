package streamtlm.bus

import chisel3._
import chisel3.util._

// AXI4 parameter
class AXI4Params(
    val addrWidth: Int = 32,
    val dataWidth: Int = 32,
    val idWidth: Int = 4,
    val awUserWidth: Int = 0,
    val arUserWidth: Int = 0,
    val wUserWidth: Int = 0,
    val rUserWidth: Int = 0,
    val bUserWidth: Int = 0,
    val isLite: Boolean = false,
    val hasCache: Boolean = false,
    val hasProt: Boolean = false,
    val hasQos: Boolean = false,
    val hasRegion: Boolean = false,
    val hasLock: Boolean = false
)

class AXI4_AR(params: AXI4Params) extends Bundle {
    val id = if (params.idWidth > 0) Some(UInt(params.idWidth.W)) else None
    val addr = UInt(params.addrWidth.W)
    val len = if (params.isLite) None else Some(UInt(8.W))
    val size = if (params.isLite) None else Some(UInt(3.W))
    val burst = if (params.isLite) None else Some(UInt(2.W))
    val lock = if (params.hasLock) Some(Bool()) else None
    val cache = if (params.hasCache) Some(UInt(4.W)) else None
    val prot = if (params.hasProt) Some(UInt(3.W)) else None
    val qos = if (params.hasQos) Some(UInt(4.W)) else None
    val region = if (params.hasRegion) Some(UInt(4.W)) else None
    val user = if (params.arUserWidth > 0) Some(UInt(params.arUserWidth.W)) else None

    def genIO(prefix: String, direction: Boolean): Seq[(Boolean, Int, String)] = {
        val res: Seq[(Boolean, Int, String)] = Seq(
            (direction, params.idWidth, s"${prefix}_ARID"),
            (direction, params.addrWidth, s"${prefix}_ARADDR"),
            (direction, if (params.isLite) 0 else 8, s"${prefix}_ARLEN"),
            (direction, if (params.isLite) 0 else 3, s"${prefix}_ARSIZE"),
            (direction, if (params.isLite) 0 else 2, s"${prefix}_ARBURST"),
            (direction, if (params.hasLock) 1 else 0, s"${prefix}_ARLOCK"),
            (direction, if (params.hasCache) 4 else 0, s"${prefix}_ARCACHE"),
            (direction, if (params.hasProt) 3 else 0, s"${prefix}_ARPROT"),
            (direction, if (params.hasQos) 4 else 0, s"${prefix}_ARQOS"),
            (direction, if (params.hasRegion) 4 else 0, s"${prefix}_ARREGION"),
            (direction, if (params.arUserWidth > 0) params.arUserWidth else 0, s"${prefix}_ARUSER"),
            (direction, 1, s"${prefix}_ARVALID"),
            (!direction, 1, s"${prefix}_ARREADY")
        ).filter(_._2 > 0)
        res
    }

    def genInstConnect(prefix: String, dstName: String): Seq[(String, String)] = {
        val validNames = Set(genIO(prefix, true).map(_._3):_*)
        val res: Seq[(String, String)] = Seq(
            (s"${dstName}_ar_bits_id", s"${prefix}_ARID"),
            (s"${dstName}_ar_bits_addr", s"${prefix}_ARADDR"),
            (s"${dstName}_ar_bits_len", s"${prefix}_ARLEN"),
            (s"${dstName}_ar_bits_size", s"${prefix}_ARSIZE"),
            (s"${dstName}_ar_bits_burst", s"${prefix}_ARBURST"),
            (s"${dstName}_ar_bits_lock", s"${prefix}_ARLOCK"),
            (s"${dstName}_ar_bits_cache", s"${prefix}_ARCACHE"),
            (s"${dstName}_ar_bits_prot", s"${prefix}_ARPROT"),
            (s"${dstName}_ar_bits_qos", s"${prefix}_ARQOS"),
            (s"${dstName}_ar_bits_region", s"${prefix}_ARREGION"),
            (s"${dstName}_ar_bits_user", s"${prefix}_ARUSER"),
            (s"${dstName}_ar_valid", s"${prefix}_ARVALID"),
            (s"${dstName}_ar_ready", s"${prefix}_ARREADY")
        ).filter(x => validNames.contains(x._2))
        res
    }
}

class AXI4_R(params: AXI4Params) extends Bundle {
    val id = UInt(params.idWidth.W)
    val data = UInt(params.dataWidth.W)
    val resp = UInt(2.W)
    val last = if (params.isLite) None else Some(Bool())
    val user = if (params.rUserWidth > 0) Some(UInt(params.rUserWidth.W)) else None

    def genIO(prefix: String, direction: Boolean): Seq[(Boolean, Int, String)] = {
        val res: Seq[(Boolean, Int, String)] = Seq(
            (direction, params.idWidth, s"${prefix}_RID"),
            (direction, params.dataWidth, s"${prefix}_RDATA"),
            (direction, 2, s"${prefix}_RRESP"),
            (direction, if (params.isLite) 0 else 1, s"${prefix}_RLAST"),
            (direction, if (params.rUserWidth > 0) params.rUserWidth else 0, s"${prefix}_RUSER"),
            (direction, 1, s"${prefix}_RVALID"),
            (!direction, 1, s"${prefix}_RREADY")
        ).filter(_._2 > 0)
        res
    }

    def genInstConnect(prefix: String, dstName: String): Seq[(String, String)] = {
        val validNames = Set(genIO(prefix, true).map(_._3):_*)
        val res: Seq[(String, String)] = Seq(
            (s"${dstName}_r_bits_id", s"${prefix}_RID"),
            (s"${dstName}_r_bits_data", s"${prefix}_RDATA"),
            (s"${dstName}_r_bits_resp", s"${prefix}_RRESP"),
            (s"${dstName}_r_bits_last", s"${prefix}_RLAST"),
            (s"${dstName}_r_bits_user", s"${prefix}_RUSER"),
            (s"${dstName}_r_valid", s"${prefix}_RVALID"),
            (s"${dstName}_r_ready", s"${prefix}_RREADY")
        ).filter(x => validNames.contains(x._2))
        res
    }
}

class AXI4_AW(params: AXI4Params) extends Bundle {
    val id = UInt(params.idWidth.W)
    val addr = UInt(params.addrWidth.W)
    val len = if (params.isLite) None else Some(UInt(8.W))
    val size = if (params.isLite) None else Some(UInt(3.W))
    val burst = if (params.isLite) None else Some(UInt(2.W))
    val lock = if (params.hasLock) Some(Bool()) else None
    val cache = if (params.hasCache) Some(UInt(4.W)) else None
    val prot = if (params.hasProt) Some(UInt(3.W)) else None
    val qos = if (params.hasQos) Some(UInt(4.W)) else None
    val region = if (params.hasRegion) Some(UInt(4.W)) else None
    val user = if (params.awUserWidth > 0) Some(UInt(params.awUserWidth.W)) else None

    def genIO(prefix: String, direction: Boolean): Seq[(Boolean, Int, String)] = {
        val res: Seq[(Boolean, Int, String)] = Seq(
            (direction, params.idWidth, s"${prefix}_AWID"),
            (direction, params.addrWidth, s"${prefix}_AWADDR"),
            (direction, if (params.isLite) 0 else 8, s"${prefix}_AWLEN"),
            (direction, if (params.isLite) 0 else 3, s"${prefix}_AWSIZE"),
            (direction, if (params.isLite) 0 else 2, s"${prefix}_AWBURST"),
            (direction, if (params.hasLock) 1 else 0, s"${prefix}_AWLOCK"),
            (direction, if (params.hasCache) 4 else 0, s"${prefix}_AWCACHE"),
            (direction, if (params.hasProt) 3 else 0, s"${prefix}_AWPROT"),
            (direction, if (params.hasQos) 4 else 0, s"${prefix}_AWQOS"),
            (direction, if (params.hasRegion) 4 else 0, s"${prefix}_AWREGION"),
            (direction, if (params.awUserWidth > 0) params.awUserWidth else 0, s"${prefix}_AWUSER"),
            (direction, 1, s"${prefix}_AWVALID"),
            (!direction, 1, s"${prefix}_AWREADY")
        ).filter(_._2 > 0)
        res
    }
    
    def genInstConnect(prefix: String, dstName: String): Seq[(String, String)] = {
        val validNames = Set(genIO(prefix, true).map(_._3):_*)
        val res: Seq[(String, String)] = Seq(
            (s"${dstName}_aw_bits_id", s"${prefix}_AWID"),
            (s"${dstName}_aw_bits_addr", s"${prefix}_AWADDR"),
            (s"${dstName}_aw_bits_len", s"${prefix}_AWLEN"),
            (s"${dstName}_aw_bits_size", s"${prefix}_AWSIZE"),
            (s"${dstName}_aw_bits_burst", s"${prefix}_AWBURST"),
            (s"${dstName}_aw_bits_lock", s"${prefix}_AWLOCK"),
            (s"${dstName}_aw_bits_cache", s"${prefix}_AWCACHE"),
            (s"${dstName}_aw_bits_prot", s"${prefix}_AWPROT"),
            (s"${dstName}_aw_bits_qos", s"${prefix}_AWQOS"),
            (s"${dstName}_aw_bits_region", s"${prefix}_AWREGION"),
            (s"${dstName}_aw_bits_user", s"${prefix}_AWUSER"),
            (s"${dstName}_aw_valid", s"${prefix}_AWVALID"),
            (s"${dstName}_aw_ready", s"${prefix}_AWREADY")
        ).filter(x => validNames.contains(x._2))
        res
    }
}

class AXI4_W(params: AXI4Params) extends Bundle {
    val data = UInt(params.dataWidth.W)
    val strb = UInt((params.dataWidth/8).W)
    val last = Bool()
    val user = if (params.wUserWidth > 0) Some(UInt(params.wUserWidth.W)) else None

    def genIO(prefix: String, direction: Boolean): Seq[(Boolean, Int, String)] = {
        val res: Seq[(Boolean, Int, String)] = Seq(
            (direction, params.dataWidth, s"${prefix}_WDATA"),
            (direction, params.dataWidth/8, s"${prefix}_WSTRB"),
            (direction, 1, s"${prefix}_WLAST"),
            (direction, if (params.wUserWidth > 0) params.wUserWidth else 0, s"${prefix}_WUSER"),
            (direction, 1, s"${prefix}_WVALID"),
            (!direction, 1, s"${prefix}_WREADY")
        ).filter(_._2 > 0)
        res
    }

    def genInstConnect(prefix: String, dstName: String): Seq[(String, String)] = {
        val validNames = Set(genIO(prefix, true).map(_._3):_*)
        val res: Seq[(String, String)] = Seq(
            (s"${dstName}_w_bits_data", s"${prefix}_WDATA"),
            (s"${dstName}_w_bits_strb", s"${prefix}_WSTRB"),
            (s"${dstName}_w_bits_last", s"${prefix}_WLAST"),
            (s"${dstName}_w_bits_user", s"${prefix}_WUSER"),
            (s"${dstName}_w_valid", s"${prefix}_WVALID"),
            (s"${dstName}_w_ready", s"${prefix}_WREADY")
        ).filter(x => validNames.contains(x._2))
        res
    }
}

class AXI4_B(params: AXI4Params) extends Bundle {
    val id = UInt(params.idWidth.W)
    val resp = UInt(2.W)
    val user = if (params.bUserWidth > 0) Some(UInt(params.bUserWidth.W)) else None

    def genIO(prefix: String, direction: Boolean): Seq[(Boolean, Int, String)] = {
        val res: Seq[(Boolean, Int, String)] = Seq(
            (direction, params.idWidth, s"${prefix}_BID"),
            (direction, 2, s"${prefix}_BRESP"),
            (direction, if (params.bUserWidth > 0) params.bUserWidth else 0, s"${prefix}_BUSER"),
            (direction, 1, s"${prefix}_BVALID"),
            (!direction, 1, s"${prefix}_BREADY")
        ).filter(_._2 > 0)
        res
    }

    def genInstConnect(prefix: String, dstName: String): Seq[(String, String)] = {
        val validNames = Set(genIO(prefix, false).map(_._3):_*)
        val res: Seq[(String, String)] = Seq(
            (s"${dstName}_b_bits_id", s"${prefix}_BID"),
            (s"${dstName}_b_bits_resp", s"${prefix}_BRESP"),
            (s"${dstName}_b_bits_user", s"${prefix}_BUSER"),
            (s"${dstName}_b_valid", s"${prefix}_BVALID"),
            (s"${dstName}_b_ready", s"${prefix}_BREADY")
        ).filter(x => validNames.contains(x._2))
        res
    }
}

class AXI4(params: AXI4Params) extends Bundle {
    // Read Address Channel
    val ar = Flipped(Decoupled(new AXI4_AR(params)))
    // Read Data Channel
    val r = Decoupled(new AXI4_R(params))
    // Write Address Channel
    val aw = Flipped(Decoupled(new AXI4_AW(params)))
    // Write Data Channel
    val w = Flipped(Decoupled(new AXI4_W(params)))
    // Write Response Channel
    val b = Decoupled(new AXI4_B(params))

    def genIO(prefix: String, direction: Boolean): Seq[(Boolean, Int, String)] = {
        val res = ar.bits.genIO(prefix, direction) ++
                  r.bits.genIO(prefix, !direction) ++
                  aw.bits.genIO(prefix, direction) ++
                  w.bits.genIO(prefix, direction) ++
                  b.bits.genIO(prefix, !direction)
        res
    }

    def genInstConnect(prefix: String, dstName: String): Seq[(String, String)] = {
        val res = ar.bits.genInstConnect(prefix, dstName) ++
                  r.bits.genInstConnect(prefix, dstName) ++
                  aw.bits.genInstConnect(prefix, dstName) ++
                  w.bits.genInstConnect(prefix, dstName) ++
                  b.bits.genInstConnect(prefix, dstName)
        res
    }
}
