/**
  * LTC RoCC Accelerator
  * 
  * Andreas Rebsamen
  */

package freechips.rocketchip.tile

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
// import freechips.rocketchip.tilelink._

import chisel3.experimental.FixedPoint

// import breeze.linalg._
import breeze.{linalg => blinag}
// import breeze.numerics._
import breeze.{numerics => bnumerics}

import scala.math._
import chisel3.experimental.ChiselAnnotation


/**
 * Accumulator Example
 *  load numbers from memory and accumulate all, return result
 */
class MyAccumulatorExample(opcodes: OpcodeSet)(implicit p: Parameters) extends LazyRoCC(opcodes) {
  override lazy val module = new MyAccumulatorExampleModuleImp(this)
}

class MyAccumulatorExampleModuleImp(outer: MyAccumulatorExample)(implicit p: Parameters) extends LazyRoCCModuleImp(outer)
    with HasCoreParameters {
  
  val cmd = io.cmd // take it directly here, no Queue for now - mybe Reg, or RegEnable would be good

  
  val base_address = Reg(UInt(xLen.W))
  val size = Reg(UInt(xLen.W))
  
  val funct = cmd.bits.inst.funct
  val s_idle :: s_rst :: s_acc :: s_waitLast :: s_resp :: Nil = Enum(5)
  val state = RegInit(s_idle)

  val doWork = state === s_acc
  val doResp = state === s_resp

  val offset = Reg(UInt(xLen.W))
  val accu = Reg(UInt(xLen.W))

  val resp_rd = Reg(chiselTypeOf(io.resp.bits.rd))
  val cmd_dprv = Reg(chiselTypeOf(cmd.bits.status.dprv))
  val cmd_dv = Reg(chiselTypeOf(cmd.bits.status.dv))

  val req_addr_valid = RegInit(false.B) // only really necessary if addess is updated on respons (see Note below)

  when (cmd.fire) {
    accu := 0.U
    state := s_rst
    offset := 0.U
    base_address := cmd.bits.rs1
    size := cmd.bits.rs2
    resp_rd := io.cmd.bits.inst.rd
    cmd_dprv := cmd.bits.status.dprv
    cmd_dv := cmd.bits.status.dv
  }

  when (state === s_rst) {
    state := s_acc
    req_addr_valid := true.B
  }

  when (io.mem.resp.valid) {
    accu := accu + io.mem.resp.bits.data
    when(io.mem.resp.bits.addr === (base_address + ((size-1.U) << 2))){  // TODO: not so nice to do addr computation here
      state := s_resp
    }
  }

  when (io.mem.req.fire) {
    req_addr_valid := false.B
  }

  // when (io.mem.req.fire && (state === s_acc)) {
  when (io.mem.resp.valid && (state === s_acc)) { // NOTE: only update on response, b.c. on request does not work somehow
    when(offset === (size-1.U)){
      // state := s_waitLast
      state := s_resp // Note: if changed on request, this shoudl be waitLast
    }.otherwise{
      offset := offset + 1.U
      req_addr_valid := true.B
    }
  }


  io.resp.valid := state === s_resp
  when (io.resp.fire) {
    state := s_idle
  }


  val stallLoad = doWork && !io.mem.req.ready
  val stallResp = doResp && !io.resp.ready

  io.busy := state =/= s_idle
  cmd.ready := !stallLoad && !stallResp && !io.busy // maybe add io.busy

//  io.resp.bits.rd := cmd.bits.inst.rd // only works if cmd is queue
  io.resp.bits.rd := resp_rd

  io.resp.bits.data := accu

  // MEMORY REQUEST INTERFACE
  // io.mem.req.valid := cmd.valid && doWork && !stallReg && !stallResp
  io.mem.req.valid := ( state === s_acc ) && req_addr_valid
  io.mem.req.bits.addr := base_address + (offset << 2)
  io.mem.req.bits.tag := 0.U
  io.mem.req.bits.cmd := M_XRD // perform a load (M_XWR for stores)
  io.mem.req.bits.size := log2Up(4).U // maybe already defined as default???
  io.mem.req.bits.signed := false.B
  io.mem.req.bits.data := 0.U // we're not performing any stores...
  io.mem.req.bits.phys := false.B
  io.mem.req.bits.dprv := cmd_dprv
  io.mem.req.bits.dv := cmd_dv

  io.interrupt := false.B
}


/**
  * LTC Accelerator 
  */

// Config Parameters (TODO: could be like Rocket-Config. Or this might be enough...)
class LTCCoprocConfig(
  val w : Int = 32, val f : Int = 16,
  // val w : Int = 16, val f : Int = 8,
  val maxNeurons: Int = 256, 
  val ramBlockArrdWidth : Int = 9,
  val hwMultWidth : Int = 18,
  val sigmoid_lut_addr_w : Int = 6, 
  val N_Units : Int = 4,
  val ltc_out_queue_size : Int = 10,
  // from core parameters
  var xLen : Int = 32,

  val DEBUG : Boolean = true
)  { 
  val neuronCounterWidth = log2Ceil(maxNeurons)
  val maxSynapses = pow(maxNeurons, 2).toInt
  val synapseCounterWidth = log2Ceil(maxSynapses)

  val UnitAddrWidth = max(1, log2Ceil(N_Units)) // requires at least one bit
  val wBytes = w / 8
}

// --- Bundels and Enums ---
object LTCUnit_WeightSel extends ChiselEnum {
  val mu, gamma, w, erev = Value

  // TODO: should generate header file for code
  for (i <- this.all) {
    val enumName = i.toString().split("=")(1).replaceAll("\\)", "")
    println(s"#define UNIT_WEIGHT_ID_${enumName} ${i.litValue}")
  }
}

class LTCUnit_WeightWrite(val w: Int, val addrWidth : Int) extends Bundle {
  val writeSelect = LTCUnit_WeightSel()
  val writeAddr = UInt(addrWidth.W)
  val writeData = SInt(w.W)
}

class LTCUnit_SparcityMatWrite(val addrWidth : Int) extends Bundle {
  val writeAddr = UInt(addrWidth.W)
  val writeData = Bool()
}

class LTCUnit_MemoryWriteIF(val w: Int, val synapseCounterWidth : Int, val ramBlockArrdWidth : Int, val neuronCounterWidth : Int) extends Bundle {
  val sparcity_write = Flipped(Valid(new LTCUnit_SparcityMatWrite(synapseCounterWidth)))
  val weight_write = Flipped(Valid(new LTCUnit_WeightWrite(w, ramBlockArrdWidth))) 

  val valids = this.getElements.toSeq.collect{ case valid: Valid[_] => valid.valid }
}

class LTCUnit_DataOut(val config : LTCCoprocConfig) extends Bundle {
  val act     = FixedPoint(config.w.W, config.f.BP)
  val rev_act = FixedPoint(config.w.W, config.f.BP)
}

class LTCCore_StateWrite(val config : LTCCoprocConfig) extends Bundle {
  val stateAddr = UInt(config.neuronCounterWidth.W)
  val stateValue = SInt(config.w.W)
}

class LTCCore_MemoryWriteIF(val config : LTCCoprocConfig) extends Bundle {
  val UnitMemWrite = new LTCUnit_MemoryWriteIF(config.w, config.synapseCounterWidth, config.ramBlockArrdWidth, config.neuronCounterWidth)
  val UnitAddr = Input(UInt(config.UnitAddrWidth.W))

  val stateWrite = Flipped(Valid(new LTCCore_StateWrite(config)))
}

object LTCUnit_CSRs extends ChiselEnum {
  val n_out_neurons, missed_out_values, result_act_addr, result_rev_act_addr = Value
  val readOnlyCSRs = List(
    missed_out_values
  )

  // TODO: should generate header file for code
  for (i <- this.all) {
    val enumName = i.toString().split("=")(1).replaceAll("\\)", "")
    println(s"#define CSR_UNIT_${enumName} ${i.litValue}")
  }
}

object LTCCore_CSRs extends ChiselEnum {
  val n_neurons, max_synapses, max_out_neurons, state_addr, n_units, enable = Value
  val readOnlyCSRs = List(
    n_units
    // TODO: maybe add other interesting config params
  )

  // TODO: should generate header file for code
  for (i <- this.all) {
    val enumName = i.toString().split("=")(1).replaceAll("\\)", "")
    println(s"#define CSR_CORE_${enumName} ${i.litValue}")
  }
}

class LTCUnit_CSRs_IO(config : LTCCoprocConfig) extends Bundle {
  val csrWrite = Flipped(Valid(UInt(config.xLen.W)))
  val csrRead = Output(UInt(config.xLen.W))
  
  val csrSel = Flipped(Valid(LTCUnit_CSRs()))
}

class LTCCore_CSRs_IO(config : LTCCoprocConfig) extends Bundle {
  val csrWrite = Flipped(Valid(UInt(config.xLen.W)))
  val csrRead = Output(UInt(config.xLen.W))
  
  val csrSel = Flipped(Valid(LTCCore_CSRs()))

  val UnitAddr = Input(UInt(config.UnitAddrWidth.W))
  val UnitCSR = new LTCUnit_CSRs_IO(config)
}

// TODO: maybe generate this from excel or just use an Enum - hard coded for now...
object LTCCoProc_FuncDef {
  val run = 0
  val load_state = 1

  val load_sparcity = 3
  val load_weight = 4

  val get_core_csr = 64
  val set_core_csr = 65
  val get_unit_csr = 66
  val set_unit_csr = 67
  
  // TODO: should generate header file for code
  println(s"#define FUNC_run           $run")
  println(s"#define FUNC_load_state    $load_state")
  println(s"#define FUNC_load_sparcity $load_sparcity")
  println(s"#define FUNC_load_weight   $load_weight")
  println(s"#define FUNC_get_core_csr  $get_core_csr")
  println(s"#define FUNC_set_core_csr  $set_core_csr")
  println(s"#define FUNC_get_unit_csr  $get_unit_csr")
  println(s"#define FUNC_set_unit_csr  $set_unit_csr")



  def isCoreCSR(func : Bits) : Bool = {
    return ((func === get_core_csr.U) || (func === set_core_csr.U))
  }
  def isUnitCSR(func : Bits) : Bool = {
    return ((func === get_unit_csr.U) || (func === set_unit_csr.U))
  }  
  def isSetCSR(func : Bits) : Bool = {
    return ((func === set_core_csr.U) || (func === set_unit_csr.U))
  }
  def isGetCSR(func : Bits) : Bool = {
    return ((func === get_core_csr.U) || (func === get_unit_csr.U))
  }
}

// --- components --- (top down)
class LTCCoProcRoCC(opcodes: OpcodeSet, config : LTCCoprocConfig)(implicit p: Parameters) extends LazyRoCC(opcodes) {
  override lazy val module = new LTCCoProcImp(this, config : LTCCoprocConfig)
}

class LTCCoProcImp(outer: LTCCoProcRoCC, config : LTCCoprocConfig)(implicit p: Parameters) extends LazyRoCCModuleImp(outer)
    with HasCoreParameters {
  // adapt config for current core parameters
  config.xLen = xLen
  println(s"using config w: ${config.w} - f: w: ${config.f}")

  dontTouch(io)    
  io <> DontCare

  val cmd = io.cmd
  // val cmd = Queue(io.cmd)
  // cmd <> DontCare

  val core = Module(new LTCCore(config))
  dontTouch(core.io)
  core.io <> DontCare
  core.io.en := false.B

  val s_idle :: s_run :: s_load_state :: s_load_sparcity :: s_load_weight :: s_resp :: s_wait_result_write :: Nil = Enum(7)
  def IsLoadState(s : UInt) : Bool = {
    ((s === s_load_state) || (s === s_load_sparcity) || (s === s_load_weight))
  }
  val state = RegInit(s_idle)

  // register required instruciton values
  val resp_rd = Reg(chiselTypeOf(io.resp.bits.rd))
  val cmd_dprv = Reg(chiselTypeOf(cmd.bits.status.dprv))
  val cmd_dv = Reg(chiselTypeOf(cmd.bits.status.dv))
  when (cmd.fire) {
    resp_rd := io.cmd.bits.inst.rd
    cmd_dprv := cmd.bits.status.dprv
    cmd_dv := cmd.bits.status.dv
  }

  val resp_data_reg = Reg(chiselTypeOf(io.resp.bits.data))

  // read and write CSRs
  // default assignement for valids // TODO: should be more generic (idea: add trait e.g. HasValid, providing a function that collects and initializes all valids)
  core.io.csr.csrSel.valid := false.B
  core.io.csr.csrWrite.valid := false.B
  core.io.csr.UnitCSR.csrSel.valid := false.B
  core.io.csr.UnitCSR.csrWrite.valid := false.B
  when (cmd.fire) {
    when (LTCCoProc_FuncDef.isCoreCSR(cmd.bits.inst.funct)) {
      state := s_resp
      core.io.csr.csrSel.bits := LTCCore_CSRs(cmd.bits.rs1(LTCCore_CSRs.getWidth-1,0))
      core.io.csr.csrSel.valid := true.B
      core.io.csr.csrWrite.bits := cmd.bits.rs2
      core.io.csr.csrWrite.valid := LTCCoProc_FuncDef.isSetCSR(cmd.bits.inst.funct)
      resp_data_reg := (core.io.csr.csrRead).suggestName("csr_core_Read_reg")
    }.elsewhen (LTCCoProc_FuncDef.isUnitCSR(cmd.bits.inst.funct)) {
      state := s_resp
      core.io.csr.UnitAddr := cmd.bits.rs1 >> (config.xLen /2)
      core.io.csr.UnitCSR.csrSel.bits := LTCUnit_CSRs(cmd.bits.rs1(LTCUnit_CSRs.getWidth-1,0))
      core.io.csr.UnitCSR.csrSel.valid := true.B
      core.io.csr.UnitCSR.csrWrite.bits := cmd.bits.rs2
      core.io.csr.UnitCSR.csrWrite.valid := LTCCoProc_FuncDef.isSetCSR(cmd.bits.inst.funct)
      resp_data_reg := (core.io.csr.UnitCSR.csrRead).suggestName("csr_unit_Read_reg")
    }
  }

  val current_memory_addr = Reg(UInt(config.xLen.W))
  val memory_read_counter = Reg(UInt(config.synapseCounterWidth.W))
  val act_rev_memory_addr = Reg(UInt(config.xLen.W))
  val final_load_addr = Reg(UInt(config.xLen.W))
  val mem_req_valid = Reg(Bool())
  val mem_write_unit_addr = Reg(chiselTypeOf(core.io.memWrite.UnitAddr))
  val mem_write_weight_sel = Reg(LTCUnit_WeightSel())

  // load state
  when (cmd.fire && (cmd.bits.inst.funct === LTCCoProc_FuncDef.load_state.U)) {
    state := s_load_state
    current_memory_addr := core.io.state_addr
    final_load_addr := core.io.state_addr + ((core.io.n_neurons-1.U) << (log2Up(config.wBytes).U)) 
    mem_req_valid := true.B
    memory_read_counter := 0.U
  }

  // load sparcity and weights
  when (cmd.fire && (cmd.bits.inst.funct === LTCCoProc_FuncDef.load_sparcity.U)) {
    state := s_load_sparcity
    current_memory_addr := cmd.bits.rs2
    final_load_addr :=  cmd.bits.rs2 + (((cmd.bits.rs1(config.xLen/2 -1,0))-1.U) << (log2Up(config.wBytes).U)) 
    mem_write_unit_addr := cmd.bits.rs1(3*config.xLen / 4 - 1, config.xLen /2)
    mem_req_valid := true.B
    memory_read_counter := 0.U
  }
  when (cmd.fire && (cmd.bits.inst.funct === LTCCoProc_FuncDef.load_weight.U)) {
    state := s_load_weight
    current_memory_addr := cmd.bits.rs2
    final_load_addr :=  cmd.bits.rs2 + (((cmd.bits.rs1(config.xLen/2 -1,0))-1.U) << (log2Up(config.wBytes).U)) 
    mem_write_unit_addr := cmd.bits.rs1(3*config.xLen / 4 - 1, config.xLen /2)
    mem_write_weight_sel := LTCUnit_WeightSel(cmd.bits.rs1(3*config.xLen/4 + LTCUnit_WeightSel.getWidth -1, 3*config.xLen/4))
    mem_req_valid := true.B
    memory_read_counter := 0.U
  }

  // run inference
  val unit_out_cnt = RegInit(VecInit.fill(config.N_Units) { (0.U(config.neuronCounterWidth.W)) })
  val result_write_pending = RegInit(false.B)
  val core_fire_evt = RegInit(false.B)
  core_fire_evt := false.B
  when (cmd.fire && (cmd.bits.inst.funct === LTCCoProc_FuncDef.run.U)) {
    state := s_run
    core_fire_evt := true.B
    unit_out_cnt.foreach(_ := 0.U)
  }

  core.io.fire := core_fire_evt
  when (core.io.done && !RegNext(core.io.done)) {
    // change state on rising edge of done (as done will stay high until next fire)
    state := Mux(result_write_pending, s_wait_result_write, s_resp)
  }

  // write ltc results
  val writing_act = RegInit(false.B) // writing rev act otherwise
  val canTakeResult = RegInit(true.B)
  val result_act_out = Reg(chiselTypeOf(core.io.data_out.bits.act))
  val result_rev_act_out = Reg(chiselTypeOf(core.io.data_out.bits.rev_act))
  core.io.data_out.ready := canTakeResult
  when (core.io.data_out.fire) {
    result_write_pending := true.B
    canTakeResult := false.B
    writing_act := true.B
    result_act_out := core.io.data_out.bits.act
    result_rev_act_out := core.io.data_out.bits.rev_act
    current_memory_addr := core.io.result_act_addr + (unit_out_cnt(core.io.chosen_out) << log2Up(config.wBytes).U) // using unit cnt directly
    act_rev_memory_addr := core.io.result_rev_act_addr + (unit_out_cnt(core.io.chosen_out) << log2Up(config.wBytes).U) // using unit cnt directly
    unit_out_cnt(core.io.chosen_out) := unit_out_cnt(core.io.chosen_out) + 1.U
    mem_req_valid := true.B
  }

  // Handle Memory request response
  when (io.mem.req.fire) {
    mem_req_valid := false.B
  }

  when (io.mem.resp.valid && IsLoadState(state)) { // NOTE: only update on response, b.c. on request does not work somehow
    when(current_memory_addr === final_load_addr){
      state := s_resp 
      resp_data_reg := 0.U // maybe better to just not do this 🥨
    }.otherwise{
      current_memory_addr := current_memory_addr + config.wBytes.U
      memory_read_counter := memory_read_counter + 1.U
      mem_req_valid := true.B
    }
  }

  when (io.mem.resp.valid && (state===s_run || state===s_wait_result_write)) {
    when (writing_act) {
      canTakeResult := false.B
      writing_act := false.B // writing rev_act next
      current_memory_addr := act_rev_memory_addr
      mem_req_valid := true.B
    }.otherwise {
      // waiting for next result
      canTakeResult := true.B
      writing_act := true.B 
      mem_req_valid := false.B
      result_write_pending := false.B
      when (state===s_wait_result_write) {
        state := s_resp
      }
    }
  }


  // connect memory response interface
  core.io.memWrite.stateWrite.valid := false.B
  core.io.memWrite.UnitMemWrite.sparcity_write.valid := false.B
  core.io.memWrite.UnitMemWrite.weight_write.valid := false.B
  switch (state) {
    is (s_load_state) {
      core.io.memWrite.stateWrite.valid := io.mem.resp.valid
      core.io.memWrite.stateWrite.bits.stateValue := io.mem.resp.bits.data.asSInt // TODO: this probably only works for 32 bit
      core.io.memWrite.stateWrite.bits.stateAddr := memory_read_counter
    } 
    is (s_load_sparcity) {
      core.io.memWrite.UnitMemWrite.sparcity_write.valid := io.mem.resp.valid
      core.io.memWrite.UnitMemWrite.sparcity_write.bits.writeData := io.mem.resp.bits.data(0) // TODO: this probably only works for 32 bit
      core.io.memWrite.UnitMemWrite.sparcity_write.bits.writeAddr := memory_read_counter
      core.io.memWrite.UnitAddr := mem_write_unit_addr
    }
    is (s_load_weight) {
      core.io.memWrite.UnitMemWrite.weight_write.valid := io.mem.resp.valid
      core.io.memWrite.UnitMemWrite.weight_write.bits.writeData := io.mem.resp.bits.data.asSInt // TODO: this probably only works for 32 bit
      core.io.memWrite.UnitMemWrite.weight_write.bits.writeAddr := memory_read_counter
      core.io.memWrite.UnitAddr := mem_write_unit_addr
      core.io.memWrite.UnitMemWrite.weight_write.bits.writeSelect := mem_write_weight_sel
    }
    is (s_run) {
    }
  }

  // Not needed --> size and appropriate shift of store data is enough  (left here for reference)
  // def getMemoryMask(invert : Boolean = false) : UInt = {
  //   var maskStr = "b"
  //   for (i <- 0 until (config.xLen/8)) {
  //     if (((config.xLen/8)-i) > (config.w/8) ) {
  //       maskStr = maskStr + "0"
  //     } else {
  //       maskStr = maskStr + "1"
  //     }
  //   }
  //   if (invert){ // kind of ugly, but at least we'll see what is done during elaburation
  //     maskStr = maskStr.replace("0","x").replace("1","0").replace("x","1")
  //   }
  //   println(s"using memory mask with invert = $invert: " + maskStr)
  //   val maskValue = maskStr.U
  //   println(s"this is value $maskValue")

  //   return maskValue
  // }

  // MEMORY REQUEST INTERFACE
  io.mem.req.valid := mem_req_valid
  io.mem.req.bits.addr := current_memory_addr
  io.mem.req.bits.tag := 0.U
  io.mem.req.bits.cmd := Mux((state===s_run || state===s_wait_result_write), M_XWR, M_XRD)
  // io.mem.req.bits.cmd := Mux((state===s_run || state===s_wait_result_write), M_PWR, M_XRD) // Not needed --> size and appropriate shift of store data is enough
  io.mem.req.bits.size := log2Up(config.wBytes).U
  io.mem.req.bits.signed := true.B
  io.mem.req.bits.data := Mux(current_memory_addr(1) && (config.w < config.xLen).B, // shift if address is not xLen-bit aligend (write will only hapen xLen-bit aligned!) NOTE: only tested for 32 and 16 bit. Probalbly need adjustment for other number formats!
                              (Mux(writing_act, result_act_out, result_rev_act_out).pad(io.mem.req.bits.data.getWidth).asInstanceOf[Bits].asUInt) << (config.xLen - config.w),
                              Mux(writing_act, result_act_out, result_rev_act_out).pad(io.mem.req.bits.data.getWidth).asInstanceOf[Bits].asUInt)
  // io.mem.req.bits.mask := Mux(current_memory_addr(1) && (config.w < config.xLen).B, getMemoryMask(true), getMemoryMask()) // Byte mask for relevant part of word (with length xLen) // Not needed --> size and appropriate shift of store data is enough
  io.mem.req.bits.phys := false.B
  io.mem.req.bits.dprv := cmd_dprv
  io.mem.req.bits.dv := cmd_dv

  // general rocc outputs
  io.busy := state =/= s_idle
  io.cmd.ready := state === s_idle

  io.resp.valid := state === s_resp
  io.resp.bits.data := resp_data_reg
  io.resp.bits.rd := resp_rd
  when (io.resp.fire) {
    state := s_idle
  }

  io.interrupt := false.B
}



class LTCCore(config : LTCCoprocConfig) extends Module {
  val io = IO(new Bundle {
      val en          = Input(Bool()) // kept here for backwards compatibility of unit tests --> better use CSR!!
      val fire        = Input(Bool())

      val busy    = Output(Bool())
      val done    = Output(Bool())

      val memWrite = new LTCCore_MemoryWriteIF(config)

      val result_act_addr = Output(UInt(config.xLen.W))
      val result_rev_act_addr = Output(UInt(config.xLen.W))
      val data_out = Decoupled(new LTCUnit_DataOut(config))
      val chosen_out = Output(UInt(config.xLen.W)) // still required for Unit Test - USE ADDR OTHERWISE!!!

      val csr = new LTCCore_CSRs_IO(config)

      // maybe there should be a generic way to access CSRs, or proc should have those as CSRs, this wasn't really the idea
      val state_addr = Output(UInt(config.xLen.W))
      val n_neurons = Output(UInt(config.xLen.W))
  })

  // component instantiation
  println("instatiate ltc_units")
  val ltc_units = (0 until config.N_Units).map{ i => Module(new LTCUnit(config, unitID=i))}.toList

  // TODO: should be removed in the end! Why is this still necessarry?
  // default assigment to remove errors during development 
  io <> DontCare
  for (u <- ltc_units) { u.io <> DontCare }

  // Init and write registers
  var csrs : Map[LTCCore_CSRs.Type, UInt] = Map()
  LTCCore_CSRs.all.foreach{
    c => csrs += (c -> {
                        if (LTCCore_CSRs.readOnlyCSRs.contains(c)) {
                          Reg(chiselTypeOf(io.csr.csrRead)).suggestName("CSR_R_" + c.toString().split("=")(1).replaceAll("\\)", ""))
                        } else {
                          RegEnable(io.csr.csrWrite.bits, 0.U, 
                          (io.csr.csrSel.bits === c) && io.csr.csrSel.valid && io.csr.csrWrite.valid).suggestName("CSR_W_" + c.toString().split("=")(1).replaceAll("\\)", ""))
                        }
                      })}
  // read registers
  LTCCore_CSRs.all.foreach{ c =>
    when ((io.csr.csrSel.bits === c) && io.csr.csrSel.valid) {
      io.csr.csrRead := csrs(c)
    }
  }
  // constant registers
  csrs(LTCCore_CSRs.n_units) := config.N_Units.U
  io.state_addr := csrs(LTCCore_CSRs.state_addr)
  io.n_neurons := csrs(LTCCore_CSRs.n_neurons)

  val first_fire_seen = RegInit(false.B)
  when (io.fire && !RegNext(io.fire)) {
    // capture first rising edge of fire - workaround b.c. there is a problem if there is a pause beween setting enable and the first fire, 
    // afterwards it shoudl be fine, b.c. done is already set and remains high until next fire 
    first_fire_seen := true.B
  }
  var enable = (io.en || csrs(LTCCore_CSRs.enable)(0)) && (first_fire_seen || io.fire)

  // state memory
  val state_mem = SyncReadMem(config.maxNeurons, FixedPoint(config.w.W, config.f.BP))
  when(io.memWrite.stateWrite.fire) {
    state_mem(io.memWrite.stateWrite.bits.stateAddr) := io.memWrite.stateWrite.bits.stateValue.asFixedPoint(config.f.BP)
  }

  // ltc unit memory write and csr interface 
  for (i <- 0 until config.N_Units) { 
    when (io.csr.UnitAddr === i.U(config.UnitAddrWidth.W)) {
      ltc_units(i).io.csr <> io.csr.UnitCSR
      ltc_units(i).io.csr.csrSel.valid := io.csr.UnitCSR.csrSel.valid
      ltc_units(i).io.csr.csrWrite.valid := io.csr.UnitCSR.csrWrite.valid
    }.otherwise {
      ltc_units(i).io.csr.csrSel.valid := false.B
      ltc_units(i).io.csr.csrWrite.valid := false.B
    }
    ltc_units(i).io.memWrite <> io.memWrite.UnitMemWrite 
    when (io.memWrite.UnitAddr === i.U(config.UnitAddrWidth.W)) {
      ltc_units(i).io.memWrite.valids.zip(io.memWrite.UnitMemWrite.valids).foreach{ case (a, b) => a := b }
    }.otherwise {
      ltc_units(i).io.memWrite.valids.foreach(_ := false.B)
    }
  }

  val units_done = Wire(Bool())

  // Counter logic
  val input_neuron_counter = RegInit(0.U(config.neuronCounterWidth.W))
  when ((input_neuron_counter === (csrs(LTCCore_CSRs.n_neurons)-1.U)) || io.fire || units_done) { 
    input_neuron_counter := 0.U
  }.elsewhen (enable) {
    input_neuron_counter := input_neuron_counter + 1.U
  }
  
  val out_neuron_counter = RegInit(0.U(config.neuronCounterWidth.W))
  when (io.fire || units_done) { // NOTE: assumes will never overflow (as it never should, otherwise config is bad)
    out_neuron_counter := 0.U
  }.elsewhen ((input_neuron_counter === (csrs(LTCCore_CSRs.n_neurons)-1.U)) && enable) {
    out_neuron_counter := out_neuron_counter + 1.U
  }

  val synapse_counter = RegInit(0.U(config.synapseCounterWidth.W))
  when (io.fire || units_done) { // NOTE: assumes will never overflow (as it never should, otherwise config is bad)
    synapse_counter := 0.U
  }.elsewhen (enable) {
    synapse_counter := synapse_counter + 1.U
  }

  // Unit Inputs
  val current_state = state_mem(input_neuron_counter)
  val fire_z1 = RegNext(io.fire)

  ltc_units.foreach{u => 
    u.io.en := enable // maybe should be delayed too, but should better be flexibel
    u.io.fire := fire_z1
    
    u.io.j := out_neuron_counter
    u.io.k := synapse_counter
    u.io.x_z1 := current_state
    u.io.last_state := (input_neuron_counter === (csrs(LTCCore_CSRs.n_neurons)-1.U)) // TODO: does this need to be delayed too?  - should actually be fine
  }

  // Unit outputs
  io.busy := ltc_units.collect(_.io.busy).reduce(_||_)
  units_done := ltc_units.collect(_.io.done).reduce(_&&_)

  val queuesEmpty = (0 until config.N_Units).map{ i => Wire(Bool())}.toList
  val result_write_arbitrer = Module(new RRArbiter(chiselTypeOf(ltc_units(0).io.unit_out.bits), config.N_Units))
  result_write_arbitrer.io <> DontCare // TODO: why is this necessary????? 😠
  for (i <- 0 until config.N_Units) {
    val enq : ReadyValidIO[LTCUnit_DataOut] = ltc_units(i).io.unit_out
    val q = Module(new Queue(chiselTypeOf(enq.bits), config.ltc_out_queue_size))
    q.io.enq.valid := enq.valid // copy from Queue implementation
    q.io.enq.bits := enq.bits
    enq.ready := q.io.enq.ready
    result_write_arbitrer.io.in(i) <> q.io.deq
    queuesEmpty(i) := (q.io.count === 0.U) // FYI: this is why the short Queue syntax is not used
  }

  io.done := units_done && queuesEmpty.reduce(_ && _) && !io.fire 

  for (i <- 0 until config.N_Units) {
    when (result_write_arbitrer.io.chosen === i.U) {
      io.result_act_addr     := ltc_units(i).io.result_act_addr
      io.result_rev_act_addr := ltc_units(i).io.result_rev_act_addr
    }
  }
  io.data_out <> result_write_arbitrer.io.out
  io.chosen_out := result_write_arbitrer.io.chosen

}

class LTCUnit(  val config  : LTCCoprocConfig, val unitID : Int = -1
              ) extends Module {

  val io = IO(new Bundle {

      val en          = Input(Bool())
      val j           = Input(UInt(config.neuronCounterWidth.W)) 
      val last_state = Input(Bool())
      val x_z1        = Input(FixedPoint(config.w.W, config.f.BP))
      val k           = Input(UInt(config.synapseCounterWidth.W))
      val fire        = Input(Bool())

      val busy    = Output(Bool())
      val done    = Output(Bool())

      val unit_out = Decoupled(new LTCUnit_DataOut(config))

      val memWrite = new LTCUnit_MemoryWriteIF(config.w, config.synapseCounterWidth, config.ramBlockArrdWidth, config.neuronCounterWidth)

      val csr = new LTCUnit_CSRs_IO(config)

      val result_act_addr = Output(UInt(config.xLen.W))
      val result_rev_act_addr = Output(UInt(config.xLen.W))
  })

  // component instantiation
  val sigmoid = Module(new HardSigmoid(config))

  // constants
  val MULT_LATENCY = if (config.w > 17) {4} else {1}
  println(s"Mult lantency is: $MULT_LATENCY")
  val LATENCY = sigmoid.LATENCY + 7 + 2*MULT_LATENCY + (MULT_LATENCY-1) // Latency input to output
  val THROUGHPUT = 1 // 1 synapse per cc

  // TODO: should be removed in the end! Why is this still necessarry?
  // default assigment to remove errors during development 
  io <> DontCare
  sigmoid.io <> DontCare

  val N_out_neurons = RegEnable(
    io.csr.csrWrite.bits, 0.U, 
    io.csr.csrWrite.valid && io.csr.csrSel.valid && (io.csr.csrSel.bits === LTCUnit_CSRs.n_out_neurons))

  val result_act_addr = RegEnable(
    io.csr.csrWrite.bits, 0.U, 
    io.csr.csrWrite.valid && io.csr.csrSel.valid && (io.csr.csrSel.bits === LTCUnit_CSRs.result_act_addr))
  io.result_act_addr := result_act_addr
  val result_rev_act_addr = RegEnable(
    io.csr.csrWrite.bits, 0.U, 
    io.csr.csrWrite.valid && io.csr.csrSel.valid && (io.csr.csrSel.bits === LTCUnit_CSRs.result_rev_act_addr))
  io.result_rev_act_addr := result_rev_act_addr

  // memory definition
  var weight_mems : Map[LTCUnit_WeightSel.Type, SyncReadMem[FixedPoint] ] = Map()
  LTCUnit_WeightSel.all.foreach{
    m => weight_mems += (m -> SyncReadMem(pow(2, config.ramBlockArrdWidth).toInt, FixedPoint(config.w.W, config.f.BP)).suggestName("MEM_" + m.toString().split("=")(1).replaceAll("\\)", "")))
  }
  val sparcity_mem = SyncReadMem(config.maxSynapses, Bool())

  // memory write 
  when(io.memWrite.weight_write.fire) {
    LTCUnit_WeightSel.all.foreach{
    m => {
      when(io.memWrite.weight_write.bits.writeSelect === m) {
        weight_mems(m)(io.memWrite.weight_write.bits.writeAddr) := io.memWrite.weight_write.bits.writeData.asFixedPoint(config.f.BP)
        }
      }
    }
  }
  when(io.memWrite.sparcity_write.fire) {
    sparcity_mem(io.memWrite.sparcity_write.bits.writeAddr) := io.memWrite.sparcity_write.bits.writeData
  }

  // event signals
  val last_neuron = Wire(Bool())
  last_neuron := (io.j >= (N_out_neurons-1.U)) && io.en
  val done_shrg = ShiftRegister(last_neuron, LATENCY+1, false.B, io.en)  // 1 cc additional latency b.c. need to wait until last neurons is actually done

  val fire_accu_rst = ShiftRegister(io.fire, LATENCY-2)
  
  val accu_rst_shrg = ShiftRegister(io.last_state, LATENCY-1, false.B, io.en)
  val accu_done = RegNext(accu_rst_shrg)
  
  val busy = RegInit(false.B)
  when(io.fire) { busy := true.B }.elsewhen(done_shrg && accu_done) { busy := false.B }

  // control
  val current_synapse_active = Wire(Bool())
  current_synapse_active := sparcity_mem(io.k) 
  val current_synapse_active_shrg = ShiftRegister(current_synapse_active, 5+MULT_LATENCY+sigmoid.LATENCY -1, io.en)

  val active_synaps_counter_next = RegInit(0.U(config.synapseCounterWidth.W))
  val active_synaps_counter = active_synaps_counter_next
  when (io.fire) {
    active_synaps_counter_next := 0.U
  }.elsewhen (current_synapse_active && io.en) {
    // inc counter, assuming overflow is impossible
    active_synaps_counter_next := active_synaps_counter_next + 1.U
  }

  // weigth address propagaion
  val mu_addr    = RegNext(active_synaps_counter)
  val gamma_addr = RegNext(mu_addr)
  val w_addr     = ShiftRegister(gamma_addr, 1+MULT_LATENCY+sigmoid.LATENCY)
  val Erev_addr  = ShiftRegister(w_addr, 1+(MULT_LATENCY-1))

  // datapath
  val x_in_shrg = ShiftRegister(io.x_z1, 2)
  val mu = weight_mems(LTCUnit_WeightSel.mu)(mu_addr)
  val x_minus_mu = RegNext(x_in_shrg - mu)
  val gamma = weight_mems(LTCUnit_WeightSel.gamma)(gamma_addr)
  val sigmoid_in = Wire(FixedPoint(config.w.W, config.f.BP))
  sigmoid_in := gamma * x_minus_mu
  sigmoid.io.x := ShiftRegister(sigmoid_in, MULT_LATENCY)
  val sigmoid_out = RegNext(sigmoid.io.y)
  val w_weight = weight_mems(LTCUnit_WeightSel.w)(w_addr)
  val s_times_w_1 = Wire(FixedPoint(config.w.W, config.f.BP))
  s_times_w_1 := Mux(
      current_synapse_active_shrg,
      sigmoid_out, 0.U.asFixedPoint(config.f.BP)) * w_weight
  val s_times_w = ShiftRegister(s_times_w_1, MULT_LATENCY)
  val E_rev = weight_mems(LTCUnit_WeightSel.erev)(Erev_addr)

  // activation accumulators
  val act_accu = Reg(chiselTypeOf(io.unit_out.bits.act))
  val rev_act_accu = Reg(chiselTypeOf(io.unit_out.bits.rev_act))

  val s_times_w_times_E_rev = Wire(FixedPoint(config.w.W, config.f.BP))
  s_times_w_times_E_rev := (s_times_w * E_rev)

  val s_times_w_reg = ShiftRegister(s_times_w, MULT_LATENCY-1)  
  val s_times_w_times_E_rev_reg = ShiftRegister(s_times_w_times_E_rev, MULT_LATENCY-1)

  when(accu_rst_shrg || fire_accu_rst) {
    act_accu     := s_times_w_reg
    rev_act_accu := s_times_w_times_E_rev_reg
  }.otherwise {
    act_accu     := act_accu     + s_times_w_reg
    rev_act_accu := rev_act_accu + s_times_w_times_E_rev_reg
  }

  val done_out = RegInit(false.B)
  when (done_shrg && accu_done) { done_out := true.B } // maybe accu_rst_shrg is better than accu_done
  // when (done_shrg && (accu_rst_shrg || accu_done)) { done_out := true.B } // maybe accu_rst_shrg is better than accu_done
  .elsewhen(io.fire) { done_out := false.B }
  io.done := done_out && !io.fire // use !io.fire to make sure output goes to low immediately on fire
  io.busy := busy

  // output
  io.unit_out.valid := accu_rst_shrg && !done_out
  when(accu_rst_shrg) {
    io.unit_out.bits.act     := act_accu
    io.unit_out.bits.rev_act := rev_act_accu
  }

  val out_missed_count = RegInit(0.U(config.xLen.W))
  if (config.DEBUG) {
    when(io.unit_out.valid && !io.unit_out.ready && io.en) {
      out_missed_count := out_missed_count + 1.U
      printf("LTC Unit output was missed!!!! \n")
    }
  }

  // this is not the proper generic way to do this, but it is done like this for historical reasons
  when (io.csr.csrSel.valid) {
    switch (io.csr.csrSel.bits) {
      is (LTCUnit_CSRs.n_out_neurons) { io.csr.csrRead := N_out_neurons }
      is (LTCUnit_CSRs.missed_out_values) { io.csr.csrRead := out_missed_count }
      is (LTCUnit_CSRs.result_act_addr) { io.csr.csrRead := result_act_addr }
      is (LTCUnit_CSRs.result_rev_act_addr) { io.csr.csrRead := result_rev_act_addr }
    }
  }  

}

class HardSigmoid(val config : LTCCoprocConfig) extends Module {
  val io = IO(new Bundle {
    val x = Input(FixedPoint(config.w.W, config.f.BP))
    val y = Output(FixedPoint(config.w.W, config.f.BP))
  })

  val LATENCY : Int = 2
  val lut_len : Int = pow(2, config.sigmoid_lut_addr_w).toInt
  val step_size : Double = 8.0/lut_len

  require(lut_len >= 8, s"HardSigmoid lut_len must be >= 8 - lut_len is $lut_len with lut_addr_w ${config.sigmoid_lut_addr_w}")

  // translated from python GenerateSigmoidErrorLut.py (thanks ChatGTP!)
  def getErrorLUT() : (Array[Double], Double, Double) = {
    // %% generate correct sigmoid
    val x = blinag.linspace(-10.0, 10.0, 100000)
    val sigmoid_f = (x: blinag.DenseVector[Double]) => 1.0 / (bnumerics.exp(-x) + 1.0)
    // val sigmoid = sigmoid_f(x)

    // %% generate hard sigmoid
    val hard_sigmoid_f = (x: blinag.DenseVector[Double]) => {
        x.map(v => {
            if (v < -2) {
                0.0
            } else if (v > 2) {
                1.0
            } else {
                0.25 * v + 0.5
            }
        })
    }
    // val hard_sigmoid = hard_sigmoid_f(x)

    // %% compute error
    val error_f = (x: blinag.DenseVector[Double]) => sigmoid_f(x) - hard_sigmoid_f(x)
    val error = error_f(x)

    // %% determine range for LUT (is fixed to interval [0,8) )
    val x_min = 0.0
    val x_max = 8.0 - step_size

    // %% determine x values for LUT
    val x_lut = blinag.linspace(x_min, x_max, lut_len)

    // %% comput erros for LUT
    val error_lut = -error_f(x_lut)

    return (error_lut.data, x_min, x_max)
  }

  // generate and quantize error LUT
  val (error_lut_d, x_min_d, x_max_d) = getErrorLUT()
  val error_lut_quant = error_lut_d.map(x => round(x*pow(2.0d,config.f)).toInt)
  val max_entry = blinag.max(blinag.DenseVector[Int](error_lut_quant))
  val rom_bit_w = log2Ceil(max_entry)
  val rom_lut_memory_bits = rom_bit_w * lut_len
  println(s"using $rom_bit_w bits for $lut_len entries in Sigmoid Error LUT --> LUT requires $rom_lut_memory_bits bits")
  println(s"Correcting Error in the range from $x_min_d to $x_max_d")

  val lut_x_shift = config.sigmoid_lut_addr_w-3 // b.c. fixed x_max of 8
  val rom_lut = VecInit(error_lut_quant.map(_.U(rom_bit_w.W)))

  val x_sign = (io.x > FixedPoint(0, config.w.W, config.f.BP))
  val x_sign_shrg = ShiftRegister(x_sign, 2, true.B)

  // linear interpolation
  val y_interpolated_raw = (io.x >> 2) + FixedPoint.fromDouble(0.5, config.w.W, config.f.BP)
  val y_interpolated = MuxCase(
    y_interpolated_raw, // default
    Array(
      (io.x < FixedPoint.fromDouble(-2.0, config.w.W, config.f.BP)) -> FixedPoint.fromDouble(0.0, config.w.W, config.f.BP),
      (io.x > FixedPoint.fromDouble(+2.0, config.w.W, config.f.BP)) -> FixedPoint.fromDouble(1.0, config.w.W, config.f.BP)
      )
    )
  val y_interpolated_shrg = ShiftRegister(y_interpolated, 2, true.B)

  // error correction
  val x_abs = io.x.abs()

  def getErrorLutAddr(x : FixedPoint ) : UInt = {
    val addr = (x.asUInt >> (config.f - lut_x_shift).asUInt) + ((x.asUInt)(config.f - lut_x_shift -1))
    Mux((x >= FixedPoint.fromDouble(8.0, config.w.W, config.f.BP)), 0.U, addr)
  }

  val addr = Reg(UInt(config.sigmoid_lut_addr_w.W))
  addr := getErrorLutAddr(x_abs)
  val e_reg = Reg(FixedPoint(config.w.W, config.f.BP))
  val e = Wire(UInt(config.w.W))
  e := rom_lut(addr)
  e_reg := e.asFixedPoint(config.f.BP) 
  // printf(cf"addr = $addr - e = $e \n")

  // output
  io.y := Mux(
        x_sign_shrg, 
          y_interpolated_shrg - e_reg, 
          y_interpolated_shrg + e_reg
      )
}
