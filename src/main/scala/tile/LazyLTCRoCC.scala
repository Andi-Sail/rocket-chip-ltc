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
  val N_PEs : Int = 4,
  val ltc_out_queue_size : Int = 10,
  // from core parameters
  var xLen : Int = 32,

  val DEBUG : Boolean = true
)  { 
  val neuronCounterWidth = log2Ceil(maxNeurons)
  val maxSynapses = pow(maxNeurons, 2).toInt
  val synapseCounterWidth = log2Ceil(maxSynapses)

  val PEAddrWidth = max(1, log2Ceil(N_PEs)) // requires at least one bit
  val wBytes = w / 8
}

// --- Bundels and Enums ---
object LTCPE_WeightSel extends ChiselEnum {
  val mu, gamma, w, erev = Value

  // TODO: should generate header file for code
  for (i <- this.all) {
    val enumName = i.toString().split("=")(1).replaceAll("\\)", "")
    println(s"#define PE_WEIGHT_ID_${enumName} ${i.litValue}")
  }
}

class LTCPE_WeightWrite(val w: Int, val addrWidth : Int) extends Bundle {
  val writeSelect = LTCPE_WeightSel()
  val writeAddr = UInt(addrWidth.W)
  val writeData = SInt(w.W)
}

class LTCPE_SparcityMatWrite(val addrWidth : Int) extends Bundle {
  val writeAddr = UInt(addrWidth.W)
  val writeData = Bool()
}

class LTCPE_MemoryWriteIF(val w: Int, val synapseCounterWidth : Int, val ramBlockArrdWidth : Int, val neuronCounterWidth : Int) extends Bundle {
  val sparcity_write = Flipped(Valid(new LTCPE_SparcityMatWrite(synapseCounterWidth)))
  val weight_write = Flipped(Valid(new LTCPE_WeightWrite(w, ramBlockArrdWidth))) 

  val valids = this.getElements.toSeq.collect{ case valid: Valid[_] => valid.valid }
}

class LTCPE_DataOut(val config : LTCCoprocConfig) extends Bundle {
  val act     = FixedPoint(config.w.W, config.f.BP)
  val rev_act = FixedPoint(config.w.W, config.f.BP)
}

class LTCCore_StateWrite(val config : LTCCoprocConfig) extends Bundle {
  val stateAddr = UInt(config.neuronCounterWidth.W)
  val stateValue = SInt(config.w.W)
}

class LTCCore_MemoryWriteIF(val config : LTCCoprocConfig) extends Bundle {
  val PEMemWrite = new LTCPE_MemoryWriteIF(config.w, config.synapseCounterWidth, config.ramBlockArrdWidth, config.neuronCounterWidth)
  val PEAddr = Input(UInt(config.PEAddrWidth.W))

  val stateWrite = Flipped(Valid(new LTCCore_StateWrite(config)))
}

object LTCPE_CSRs extends ChiselEnum {
  val n_out_neurons, missed_out_values, result_act_addr, result_rev_act_addr = Value
  val readOnlyCSRs = List(
    missed_out_values
  )

  // TODO: should generate header file for code
  for (i <- this.all) {
    val enumName = i.toString().split("=")(1).replaceAll("\\)", "")
    println(s"#define CSR_PE_${enumName} ${i.litValue}")
  }
}

object LTCCore_CSRs extends ChiselEnum {
  val n_neurons, max_synapses, max_out_neurons, state_addr, n_pes, enable = Value
  val readOnlyCSRs = List(
    n_pes
    // TODO: maybe add other interesting config params
  )

  // TODO: should generate header file for code
  for (i <- this.all) {
    val enumName = i.toString().split("=")(1).replaceAll("\\)", "")
    println(s"#define CSR_CORE_${enumName} ${i.litValue}")
  }
}

class LTCPE_CSRs_IO(config : LTCCoprocConfig) extends Bundle {
  val csrWrite = Flipped(Valid(UInt(config.xLen.W)))
  val csrRead = Output(UInt(config.xLen.W))
  
  val csrSel = Flipped(Valid(LTCPE_CSRs()))
}

class LTCCore_CSRs_IO(config : LTCCoprocConfig) extends Bundle {
  val csrWrite = Flipped(Valid(UInt(config.xLen.W)))
  val csrRead = Output(UInt(config.xLen.W))
  
  val csrSel = Flipped(Valid(LTCCore_CSRs()))

  val PEAddr = Input(UInt(config.PEAddrWidth.W))
  val PECSR = new LTCPE_CSRs_IO(config)
}

// TODO: maybe generate this from excel or just use an Enum - hard coded for now...
object LTCCoProc_FuncDef {
  val run = 0
  val load_state = 1

  val load_sparcity = 3
  val load_weight = 4

  val get_core_csr = 64
  val set_core_csr = 65
  val get_pe_csr = 66
  val set_pe_csr = 67
  
  // TODO: should generate header file for code
  println(s"#define FUNC_run           $run")
  println(s"#define FUNC_load_state    $load_state")
  println(s"#define FUNC_load_sparcity $load_sparcity")
  println(s"#define FUNC_load_weight   $load_weight")
  println(s"#define FUNC_get_core_csr  $get_core_csr")
  println(s"#define FUNC_set_core_csr  $set_core_csr")
  println(s"#define FUNC_get_pe_csr  $get_pe_csr")
  println(s"#define FUNC_set_pe_csr  $set_pe_csr")



  def isCoreCSR(func : Bits) : Bool = {
    return ((func === get_core_csr.U) || (func === set_core_csr.U))
  }
  def isPECSR(func : Bits) : Bool = {
    return ((func === get_pe_csr.U) || (func === set_pe_csr.U))
  }  
  def isSetCSR(func : Bits) : Bool = {
    return ((func === set_core_csr.U) || (func === set_pe_csr.U))
  }
  def isGetCSR(func : Bits) : Bool = {
    return ((func === get_core_csr.U) || (func === get_pe_csr.U))
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

  io <> DontCare

  val cmd = io.cmd

  val core = Module(new LTCCore(config))
  core.io <> DontCare
  core.io.en := false.B

  // CoProc state
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

  // --- read and write CSRs ---
  // default assignement for valids // TODO: should be more generic (idea: add trait e.g. HasValid, providing a function that collects and initializes all valids)
  core.io.csr.csrSel.valid := false.B
  core.io.csr.csrWrite.valid := false.B
  core.io.csr.PECSR.csrSel.valid := false.B
  core.io.csr.PECSR.csrWrite.valid := false.B
  when (cmd.fire) {
    when (LTCCoProc_FuncDef.isCoreCSR(cmd.bits.inst.funct)) {
      state := s_resp
      core.io.csr.csrSel.bits := LTCCore_CSRs(cmd.bits.rs1(LTCCore_CSRs.getWidth-1,0))
      core.io.csr.csrSel.valid := true.B
      core.io.csr.csrWrite.bits := cmd.bits.rs2
      core.io.csr.csrWrite.valid := LTCCoProc_FuncDef.isSetCSR(cmd.bits.inst.funct)
      resp_data_reg := (core.io.csr.csrRead).suggestName("csr_core_Read_reg")
    }.elsewhen (LTCCoProc_FuncDef.isPECSR(cmd.bits.inst.funct)) {
      state := s_resp
      core.io.csr.PEAddr := cmd.bits.rs1 >> (config.xLen /2)
      core.io.csr.PECSR.csrSel.bits := LTCPE_CSRs(cmd.bits.rs1(LTCPE_CSRs.getWidth-1,0))
      core.io.csr.PECSR.csrSel.valid := true.B
      core.io.csr.PECSR.csrWrite.bits := cmd.bits.rs2
      core.io.csr.PECSR.csrWrite.valid := LTCCoProc_FuncDef.isSetCSR(cmd.bits.inst.funct)
      resp_data_reg := (core.io.csr.PECSR.csrRead).suggestName("csr_pe_Read_reg")
    }
  }

  val current_memory_addr = Reg(UInt(config.xLen.W))
  val memory_read_counter = Reg(UInt(config.synapseCounterWidth.W))
  val act_rev_memory_addr = Reg(UInt(config.xLen.W))
  val final_load_addr = Reg(UInt(config.xLen.W))
  val mem_req_valid = Reg(Bool())
  val mem_write_pe_addr = Reg(chiselTypeOf(core.io.memWrite.PEAddr))
  val mem_write_weight_sel = Reg(LTCPE_WeightSel())

  // --- Memory read interface for weights ---
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
    mem_write_pe_addr := cmd.bits.rs1(3*config.xLen / 4 - 1, config.xLen /2)
    mem_req_valid := true.B
    memory_read_counter := 0.U
  }
  when (cmd.fire && (cmd.bits.inst.funct === LTCCoProc_FuncDef.load_weight.U)) {
    state := s_load_weight
    current_memory_addr := cmd.bits.rs2
    final_load_addr :=  cmd.bits.rs2 + (((cmd.bits.rs1(config.xLen/2 -1,0))-1.U) << (log2Up(config.wBytes).U)) 
    mem_write_pe_addr := cmd.bits.rs1(3*config.xLen / 4 - 1, config.xLen /2)
    mem_write_weight_sel := LTCPE_WeightSel(cmd.bits.rs1(3*config.xLen/4 + LTCPE_WeightSel.getWidth -1, 3*config.xLen/4))
    mem_req_valid := true.B
    memory_read_counter := 0.U
  }

  // --- Control Logic for inference ---
  // run inference
  val pe_out_cnt = RegInit(VecInit.fill(config.N_PEs) { (0.U(config.neuronCounterWidth.W)) })
  val result_write_pending = RegInit(false.B)
  val core_fire_evt = RegInit(false.B)
  core_fire_evt := false.B
  when (cmd.fire && (cmd.bits.inst.funct === LTCCoProc_FuncDef.run.U)) {
    state := s_run
    core_fire_evt := true.B
    pe_out_cnt.foreach(_ := 0.U)
  }

  core.io.fire := core_fire_evt
  when (core.io.done && !RegNext(core.io.done)) {
    // change state on rising edge of done (as done will stay high until next fire)
    state := Mux(result_write_pending, s_wait_result_write, s_resp)
  }

  // --- Memory write interface for results ---
  // write ltc results
  val writing_act = RegInit(false.B) // writing rev act otherwise
  val canTakeResult = RegInit(true.B) // indicating wether memory write interface is ready for the next result
  core.io.data_out.ready := canTakeResult
  val result_act_out = Reg(chiselTypeOf(core.io.data_out.bits.act))
  val result_rev_act_out = Reg(chiselTypeOf(core.io.data_out.bits.rev_act))
  when (core.io.data_out.fire) {
    result_write_pending := true.B
    canTakeResult := false.B
    writing_act := true.B // always write act first
    result_act_out := core.io.data_out.bits.act
    result_rev_act_out := core.io.data_out.bits.rev_act
    current_memory_addr := core.io.result_act_addr + (pe_out_cnt(core.io.chosen_out) << log2Up(config.wBytes).U) // using pe cnt directly as result address offset
    act_rev_memory_addr := core.io.result_rev_act_addr + (pe_out_cnt(core.io.chosen_out) << log2Up(config.wBytes).U) 
    pe_out_cnt(core.io.chosen_out) := pe_out_cnt(core.io.chosen_out) + 1.U
    mem_req_valid := true.B
  }

  // Handle Memory request and response
  when (io.mem.req.fire) {
    // make sure requests never come directly after each other (memory interface is losing data otherwise, might be a bug in the memory interface)
    //     (that is why the request interface is only updated when the response arrives, as done below)
    mem_req_valid := false.B
  }

  // response for reading from memory
  when (io.mem.resp.valid && IsLoadState(state)) { // NOTE: only update on response, b.c. on request does not work somehow
    when(current_memory_addr === final_load_addr){
      state := s_resp 
      resp_data_reg := 0.U // maybe better to just not do this 🥨 maybe not necessary
    }.otherwise{
      current_memory_addr := current_memory_addr + config.wBytes.U // memory read address
      memory_read_counter := memory_read_counter + 1.U // address / index in the memory blocks
      mem_req_valid := true.B
    }
  }

  // response for writing to memory
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


  // --- connect memory response interface ---
  // default assignement for valids
  core.io.memWrite.stateWrite.valid := false.B
  core.io.memWrite.PEMemWrite.sparcity_write.valid := false.B
  core.io.memWrite.PEMemWrite.weight_write.valid := false.B
  switch (state) {
    is (s_load_state) {
      core.io.memWrite.stateWrite.valid := io.mem.resp.valid
      core.io.memWrite.stateWrite.bits.stateValue := io.mem.resp.bits.data.asSInt // Note: response is shifted and masked according to req.size
      core.io.memWrite.stateWrite.bits.stateAddr := memory_read_counter
    } 
    is (s_load_sparcity) {
      core.io.memWrite.PEMemWrite.sparcity_write.valid := io.mem.resp.valid
      core.io.memWrite.PEMemWrite.sparcity_write.bits.writeData := io.mem.resp.bits.data(0) // Note: response is shifted and masked according to req.size
      core.io.memWrite.PEMemWrite.sparcity_write.bits.writeAddr := memory_read_counter
      core.io.memWrite.PEAddr := mem_write_pe_addr
    }
    is (s_load_weight) {
      core.io.memWrite.PEMemWrite.weight_write.valid := io.mem.resp.valid
      core.io.memWrite.PEMemWrite.weight_write.bits.writeData := io.mem.resp.bits.data.asSInt // Note: response is shifted and masked according to req.size
      core.io.memWrite.PEMemWrite.weight_write.bits.writeAddr := memory_read_counter
      core.io.memWrite.PEAddr := mem_write_pe_addr
      core.io.memWrite.PEMemWrite.weight_write.bits.writeSelect := mem_write_weight_sel
    }
    is (s_run) { // no need to connect the response interface for writing memory (as there is no data in the response, control signals are used in logic above)
    }
  }

  // --- connecting memory request interface ---
  io.mem.req.valid := mem_req_valid
  io.mem.req.bits.addr := current_memory_addr
  io.mem.req.bits.tag := 0.U
  io.mem.req.bits.cmd := Mux((state===s_run || state===s_wait_result_write), M_XWR, M_XRD)
  io.mem.req.bits.size := log2Up(config.wBytes).U // will shift and mask response data in reading, shift is still required for writing (see below)
  io.mem.req.bits.signed := true.B
  io.mem.req.bits.data := Mux(current_memory_addr(1) && (config.w < config.xLen).B, // shift if address is not xLen-bit aligend (write will only hapen xLen-bit aligned!) NOTE: only tested for 32 and 16 bit. Probalbly need adjustment for other number formats!
                              (Mux(writing_act, result_act_out, result_rev_act_out).pad(io.mem.req.bits.data.getWidth).asInstanceOf[Bits].asUInt) << (config.xLen - config.w),
                              Mux(writing_act, result_act_out, result_rev_act_out).pad(io.mem.req.bits.data.getWidth).asInstanceOf[Bits].asUInt)
  io.mem.req.bits.phys := false.B
  io.mem.req.bits.dprv := cmd_dprv
  io.mem.req.bits.dv := cmd_dv

  // --- general rocc outputs ---
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
      val data_out = Decoupled(new LTCPE_DataOut(config))
      val chosen_out = Output(UInt(config.xLen.W)) // still required for unit Test - USE ADDR OTHERWISE!!!

      val csr = new LTCCore_CSRs_IO(config)

      // maybe there should be a generic way to access CSRs, or proc should have those as CSRs, this wasn't really the idea
      val state_addr = Output(UInt(config.xLen.W))
      val n_neurons = Output(UInt(config.xLen.W))
  })

  // component instantiation
  val ltc_pes = (0 until config.N_PEs).map{ i => Module(new LTCPE(config, peID=i))}.toList

  // TODO: should be removed in the end! Why is this still necessarry?
  // default assigment to remove errors during development 
  io <> DontCare
  for (u <- ltc_pes) { u.io <> DontCare }

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
  csrs(LTCCore_CSRs.n_pes) := config.N_PEs.U
  io.state_addr := csrs(LTCCore_CSRs.state_addr)
  io.n_neurons := csrs(LTCCore_CSRs.n_neurons)

  val first_fire_seen = RegInit(false.B)
  when (io.fire && !RegNext(io.fire)) {
    // capture first rising edge of fire - workaround b.c. there is a problem if there is a pause beween setting enable and the first fire, 
    // afterwards it should be fine, b.c. done is already set and remains high until next fire 
    first_fire_seen := true.B
  }
  var enable = (io.en || csrs(LTCCore_CSRs.enable)(0)) && (first_fire_seen || io.fire)

  // state memory
  val state_mem = SyncReadMem(config.maxNeurons, FixedPoint(config.w.W, config.f.BP))
  when(io.memWrite.stateWrite.fire) {
    state_mem(io.memWrite.stateWrite.bits.stateAddr) := io.memWrite.stateWrite.bits.stateValue.asFixedPoint(config.f.BP)
  }

  // ltc pe memory write and csr interface 
  for (i <- 0 until config.N_PEs) { 
    when (io.csr.PEAddr === i.U(config.PEAddrWidth.W)) {
      ltc_pes(i).io.csr <> io.csr.PECSR
      ltc_pes(i).io.csr.csrSel.valid := io.csr.PECSR.csrSel.valid
      ltc_pes(i).io.csr.csrWrite.valid := io.csr.PECSR.csrWrite.valid
    }.otherwise {
      ltc_pes(i).io.csr.csrSel.valid := false.B
      ltc_pes(i).io.csr.csrWrite.valid := false.B
    }
    ltc_pes(i).io.memWrite <> io.memWrite.PEMemWrite 
    when (io.memWrite.PEAddr === i.U(config.PEAddrWidth.W)) {
      ltc_pes(i).io.memWrite.valids.zip(io.memWrite.PEMemWrite.valids).foreach{ case (a, b) => a := b }
    }.otherwise {
      ltc_pes(i).io.memWrite.valids.foreach(_ := false.B)
    }
  }

  val pes_done = Wire(Bool())

  // Counter logic
  val input_neuron_counter = RegInit(0.U(config.neuronCounterWidth.W))
  when ((input_neuron_counter === (csrs(LTCCore_CSRs.n_neurons)-1.U)) || io.fire || pes_done) { 
    input_neuron_counter := 0.U
  }.elsewhen (enable) {
    input_neuron_counter := input_neuron_counter + 1.U
  }
  
  val out_neuron_counter = RegInit(0.U(config.neuronCounterWidth.W))
  when (io.fire || pes_done) { // NOTE: assumes will never overflow (as it never should, otherwise config is bad)
    out_neuron_counter := 0.U
  }.elsewhen ((input_neuron_counter === (csrs(LTCCore_CSRs.n_neurons)-1.U)) && enable) {
    out_neuron_counter := out_neuron_counter + 1.U
  }

  val synapse_counter = RegInit(0.U(config.synapseCounterWidth.W))
  when (io.fire || pes_done) { // NOTE: assumes will never overflow (as it never should, otherwise config is bad)
    synapse_counter := 0.U
  }.elsewhen (enable) {
    synapse_counter := synapse_counter + 1.U
  }

  // PE Inputs
  val current_state = state_mem(input_neuron_counter)
  val fire_z1 = RegNext(io.fire)

  ltc_pes.foreach{u => 
    u.io.en := enable
    u.io.fire := fire_z1
    
    u.io.j := out_neuron_counter
    u.io.k := synapse_counter
    u.io.x_z1 := current_state
    u.io.last_state := (input_neuron_counter === (csrs(LTCCore_CSRs.n_neurons)-1.U))
  }

  // PE outputs
  io.busy := ltc_pes.collect(_.io.busy).reduce(_||_)
  pes_done := ltc_pes.collect(_.io.done).reduce(_&&_)

  // the queue outputs are all connected to a Round-Robin Arbitrer 
  val queuesEmpty = (0 until config.N_PEs).map{ i => Wire(Bool())}.toList
  val result_write_arbitrer = Module(new RRArbiter(chiselTypeOf(ltc_pes(0).io.pe_out.bits), config.N_PEs))
  result_write_arbitrer.io <> DontCare // TODO: why is this necessary????? 😠
  for (i <- 0 until config.N_PEs) {
    val enq : ReadyValidIO[LTCPE_DataOut] = ltc_pes(i).io.pe_out
    val q = Module(new Queue(chiselTypeOf(enq.bits), config.ltc_out_queue_size))
    q.io.enq.valid := enq.valid // copy from Queue implementation
    q.io.enq.bits := enq.bits
    enq.ready := q.io.enq.ready
    result_write_arbitrer.io.in(i) <> q.io.deq
    queuesEmpty(i) := (q.io.count === 0.U) // FYI: this is why the short Queue syntax is not used
  }

  // connect result address according to the chosen pe of the arbitrer
  for (i <- 0 until config.N_PEs) {
    when (result_write_arbitrer.io.chosen === i.U) {
      io.result_act_addr     := ltc_pes(i).io.result_act_addr
      io.result_rev_act_addr := ltc_pes(i).io.result_rev_act_addr
    }
  }
  // output signals
  io.done := pes_done && queuesEmpty.reduce(_ && _) && !io.fire 
  io.data_out <> result_write_arbitrer.io.out
  io.chosen_out := result_write_arbitrer.io.chosen
}

class LTCPE(  val config  : LTCCoprocConfig, val peID : Int = -1
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

      val pe_out = Decoupled(new LTCPE_DataOut(config))

      val memWrite = new LTCPE_MemoryWriteIF(config.w, config.synapseCounterWidth, config.ramBlockArrdWidth, config.neuronCounterWidth)

      val csr = new LTCPE_CSRs_IO(config)

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

  // CSR Registers instanciation and write
  val N_out_neurons = RegEnable(
    io.csr.csrWrite.bits, 0.U, 
    io.csr.csrWrite.valid && io.csr.csrSel.valid && (io.csr.csrSel.bits === LTCPE_CSRs.n_out_neurons))
  val result_act_addr = RegEnable(
    io.csr.csrWrite.bits, 0.U, 
    io.csr.csrWrite.valid && io.csr.csrSel.valid && (io.csr.csrSel.bits === LTCPE_CSRs.result_act_addr))
  io.result_act_addr := result_act_addr
  val result_rev_act_addr = RegEnable(
    io.csr.csrWrite.bits, 0.U, 
    io.csr.csrWrite.valid && io.csr.csrSel.valid && (io.csr.csrSel.bits === LTCPE_CSRs.result_rev_act_addr))
  io.result_rev_act_addr := result_rev_act_addr

  // memory definition
  var weight_mems : Map[LTCPE_WeightSel.Type, SyncReadMem[FixedPoint] ] = Map()
  LTCPE_WeightSel.all.foreach{
    m => weight_mems += (m -> SyncReadMem(pow(2, config.ramBlockArrdWidth).toInt, FixedPoint(config.w.W, config.f.BP)).suggestName("MEM_" + m.toString().split("=")(1).replaceAll("\\)", "")))
  }
  val sparcity_mem = SyncReadMem(config.maxSynapses, Bool())

  // memory write 
  when(io.memWrite.weight_write.fire) {
    LTCPE_WeightSel.all.foreach{
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

  // control logic
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
  val mu = weight_mems(LTCPE_WeightSel.mu)(mu_addr)
  val x_minus_mu = RegNext(x_in_shrg - mu)
  val gamma = weight_mems(LTCPE_WeightSel.gamma)(gamma_addr)
  val sigmoid_in = Wire(FixedPoint(config.w.W, config.f.BP))
  sigmoid_in := gamma * x_minus_mu
  sigmoid.io.x := ShiftRegister(sigmoid_in, MULT_LATENCY)
  val sigmoid_out = RegNext(sigmoid.io.y)
  val w_weight = weight_mems(LTCPE_WeightSel.w)(w_addr)
  val s_times_w_1 = Wire(FixedPoint(config.w.W, config.f.BP))
  s_times_w_1 := Mux(
      current_synapse_active_shrg,
      sigmoid_out, 0.U.asFixedPoint(config.f.BP)) * w_weight
  val s_times_w = ShiftRegister(s_times_w_1, MULT_LATENCY)
  val E_rev = weight_mems(LTCPE_WeightSel.erev)(Erev_addr)

  val s_times_w_times_E_rev = Wire(FixedPoint(config.w.W, config.f.BP))
  s_times_w_times_E_rev := (s_times_w * E_rev)

  val s_times_w_reg = ShiftRegister(s_times_w, MULT_LATENCY-1)  
  val s_times_w_times_E_rev_reg = ShiftRegister(s_times_w_times_E_rev, MULT_LATENCY-1)

  // activation accumulators
  val act_accu = Reg(chiselTypeOf(io.pe_out.bits.act))
  val rev_act_accu = Reg(chiselTypeOf(io.pe_out.bits.rev_act))
  when(accu_rst_shrg || fire_accu_rst) {
    act_accu     := s_times_w_reg
    rev_act_accu := s_times_w_times_E_rev_reg
  }.otherwise {
    act_accu     := act_accu     + s_times_w_reg
    rev_act_accu := rev_act_accu + s_times_w_times_E_rev_reg
  }

  // control output
  val done_out = RegInit(false.B)
  when (done_shrg && accu_done) { done_out := true.B }
  .elsewhen(io.fire) { done_out := false.B }
  io.done := done_out && !io.fire // use !io.fire to make sure output goes to low immediately on fire
  io.busy := busy

  // data output
  io.pe_out.valid := accu_rst_shrg && !done_out
  when(accu_rst_shrg) {
    io.pe_out.bits.act     := act_accu
    io.pe_out.bits.rev_act := rev_act_accu
  }

  val out_missed_count = RegInit(0.U(config.xLen.W))
  if (config.DEBUG) {
    when(io.pe_out.valid && !io.pe_out.ready && io.en) {
      out_missed_count := out_missed_count + 1.U
      printf("LTC PE output was missed!!!! \n")
    }
  }

  // CSR Read
  // this is not the proper generic way to do this, but it is done like this for historical reasons
  when (io.csr.csrSel.valid) {
    switch (io.csr.csrSel.bits) {
      is (LTCPE_CSRs.n_out_neurons) { io.csr.csrRead := N_out_neurons }
      is (LTCPE_CSRs.missed_out_values) { io.csr.csrRead := out_missed_count }
      is (LTCPE_CSRs.result_act_addr) { io.csr.csrRead := result_act_addr }
      is (LTCPE_CSRs.result_rev_act_addr) { io.csr.csrRead := result_rev_act_addr }
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
