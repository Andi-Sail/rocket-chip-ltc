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
  val maxNeurons: Int = 256, 
  val ramBlockArrdWidth : Int = 9,
  val hwMultWidth : Int = 18,
  val sigmoid_lut_addr_w : Int = 6, 
  val N_Units : Int = 4,
  val ltc_out_queue_size : Int = 10,
  // from core parameters
  val xLen : Int = 32,

  val DEBUG : Boolean = true
)  { 
  val neuronCounterWidth = log2Ceil(maxNeurons)
  val maxSynapses = pow(maxNeurons, 2).toInt
  val synapseCounterWidth = log2Ceil(maxSynapses)

  val UnitAddrWidth = log2Ceil(N_Units)
}

// --- Bundels and Enums ---
object LTCUnit_WeightSel extends ChiselEnum {
  val mu, gamma, w, erev = Value
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
  val N_out_neurons_write = Flipped(Valid(UInt(neuronCounterWidth.W)))
  val sparcity_write = Flipped(Valid(new LTCUnit_SparcityMatWrite(synapseCounterWidth)))
  val weight_write = Flipped(Valid(new LTCUnit_WeightWrite(w, ramBlockArrdWidth))) 

  val valids = this.getElements.toSeq.collect{ case valid: Valid[_] => valid.valid }
}

class LTCUnit_DataOut(val config : LTCCoprocConfig) extends Bundle {
  val act     = FixedPoint(config.w.W, config.f.BP)
  val rev_act = FixedPoint(config.w.W, config.f.BP)
}

class LTCUnit_CSR(val config : LTCCoprocConfig) extends Bundle {
  val unit_out_missed_count = Output(UInt(config.xLen.W))
}

class LTCCore_StateWrite(val config : LTCCoprocConfig) extends Bundle {
  val stateAddr = UInt(config.neuronCounterWidth.W)
  val stateValue = SInt(config.w.W)
}

class LTCCore_MemoryWriteIF(val config : LTCCoprocConfig) extends Bundle {
  val UnitMemWrite = new LTCUnit_MemoryWriteIF(config.w, config.synapseCounterWidth, config.ramBlockArrdWidth, config.neuronCounterWidth)
  val UnitAddr = Input(UInt(config.UnitAddrWidth.W))

  val stateWrite = Flipped(Valid(new LTCCore_StateWrite(config)))

  val N_Neurons_write = Flipped(Valid(UInt(config.neuronCounterWidth.W)))
  val Max_out_Neurons_write = Flipped(Valid(UInt(config.neuronCounterWidth.W)))
  val Max_synapses_write = Flipped(Valid(UInt(config.synapseCounterWidth.W)))
  val result_addr = Flipped(Valid(UInt(config.xLen.W)))
}

class LTCCore_CSR(val config : LTCCoprocConfig) extends Bundle {
  val units_csr = (0 until config.N_Units).map { i => new LTCUnit_CSR(config) }.toList
  // (0 until config.N_Units).map{ i => Module(new LTCUnit(config, unitID=i))}.toList
}


// --- components --- (top down)

class LTCCore(config : LTCCoprocConfig) extends Module {
  val io = IO(new Bundle {
      val en          = Input(Bool())
      val fire        = Input(Bool())

      val busy    = Output(Bool())
      val done    = Output(Bool())

      val memWrite = new LTCCore_MemoryWriteIF(config)

      // temp test outputs
      val chosen_out = Output(UInt(32.W))
      val data_out = Output(new LTCUnit_DataOut(config))
      val valid_out = Output(Bool())
  })

  // component instantiation
  println("instatiate ltc_units")
  val ltc_units = (0 until config.N_Units).map{ i => Module(new LTCUnit(config, unitID=i))}.toList

  // TODO: should be removed in the end! Why is this still necessarry?
  // default assigment to remove errors during development 
  io <> DontCare
  for (u <- ltc_units) { u.io <> DontCare }

  // config write
  val N_Neurons       = RegEnable(io.memWrite.N_Neurons_write.bits, 0.U, io.memWrite.N_Neurons_write.valid)
  val Max_out_Neurons = RegEnable(io.memWrite.Max_out_Neurons_write.bits, 0.U, io.memWrite.Max_out_Neurons_write.valid)
  val Max_synapses    = RegEnable(io.memWrite.Max_synapses_write.bits, 0.U, io.memWrite.Max_synapses_write.valid) // TODO: is this necessary??


  // state memory
  val state_mem = SyncReadMem(config.maxNeurons, FixedPoint(config.w.W, config.f.BP))
  when(io.memWrite.stateWrite.fire) {
    state_mem(io.memWrite.stateWrite.bits.stateAddr) := io.memWrite.stateWrite.bits.stateValue.asFixedPoint(config.f.BP)
  }

  // ltc unit memory write interface 
  for (i <- 0 until config.N_Units) { 
    ltc_units(i).io.memWrite <> io.memWrite.UnitMemWrite 
    when (io.memWrite.UnitAddr === i.U(config.UnitAddrWidth.W)) {
      ltc_units(i).io.memWrite.valids.zip(io.memWrite.UnitMemWrite.valids).foreach{ case (a, b) => a := b }
    }.otherwise {
      ltc_units(i).io.memWrite.valids.foreach(_ := false.B)
    }
  }

  // Counter logic
  val input_neuron_counter = RegInit(0.U(config.neuronCounterWidth.W))
  when ((input_neuron_counter === (N_Neurons-1.U)) || io.fire) { 
    input_neuron_counter := 0.U
  }.elsewhen (io.en) {
    input_neuron_counter := input_neuron_counter + 1.U
  }
  
  val out_neuron_counter = RegInit(0.U(config.neuronCounterWidth.W))
  when (io.fire) { // NOTE: assumes will never overflow (as it never should, otherwise config is bad)
    out_neuron_counter := 0.U
  }.elsewhen ((input_neuron_counter === (N_Neurons-1.U)) && io.en) {
    out_neuron_counter := out_neuron_counter + 1.U
  }

  val synapse_counter = RegInit(0.U(config.synapseCounterWidth.W))
  when (io.fire) { // NOTE: assumes will never overflow (as it never should, otherwise config is bad)
    synapse_counter := 0.U
  }.elsewhen (io.en) {
    synapse_counter := synapse_counter + 1.U
  }

  // Unit Inputs
  val current_state = state_mem(input_neuron_counter)
  val fire_z1 = RegNext(io.fire)

  ltc_units.foreach{u => 
    u.io.en := io.en // TODO: does this need to be delayed too?  
    u.io.fire := fire_z1
    u.io.j := out_neuron_counter
    u.io.k := synapse_counter
    u.io.x_z1 := current_state
    u.io.last_state := (input_neuron_counter === (N_Neurons-1.U)) // TODO: does this need to be delayed too?  
  }

  // Unit outputs
  io.busy := ltc_units.collect(_.io.busy).reduce(_||_)
  val units_done = ltc_units.collect(_.io.done).reduce(_&&_)

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

  io.done := units_done && queuesEmpty.reduce(_ && _) // TODO: maybe should incorperate other things here!

  result_write_arbitrer.io.out.ready := true.B
  io.valid_out  := false.B
  when(result_write_arbitrer.io.out.fire) {
    io.chosen_out := result_write_arbitrer.io.chosen
    io.data_out   := result_write_arbitrer.io.out.bits
    io.valid_out  := true.B
  }

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

      val csr = new LTCUnit_CSR(config)
  })

  // component instantiation
  val sigmoid = Module(new HardSigmoid(config))

  // constants
  val MULT_LATENCY = if (config.w > 17) {4} else {1}
  // val MULT_LATENCY = 1
  println(s"Mult lantency is: $MULT_LATENCY")
  val LATENCY = sigmoid.LATENCY + 8 + 2*MULT_LATENCY + (MULT_LATENCY-1) // Latency input to output
  val THROUGHPUT = 1 // 1 synapse per cc

  // TODO: should be removed in the end! Why is this still necessarry?
  // default assigment to remove errors during development 
  io <> DontCare
  sigmoid.io <> DontCare

  val N_out_neurons = RegEnable(io.memWrite.N_out_neurons_write.bits, 0.U, io.memWrite.N_out_neurons_write.valid)

  // memory definition
  var weight_mems : Map[LTCUnit_WeightSel.Type, SyncReadMem[FixedPoint] ] = Map()
  LTCUnit_WeightSel.all.foreach{
    m => weight_mems += (m -> SyncReadMem(pow(2, config.ramBlockArrdWidth).toInt, FixedPoint(config.w.W, config.f.BP)))
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
  last_neuron := (io.j >= (N_out_neurons-1.U))
  val done_shrg = ShiftRegister(last_neuron, LATENCY, io.en) 
  
  val accu_rst_shrg = ShiftRegister(io.last_state, LATENCY-1, io.en)
  val accu_done = RegNext(accu_rst_shrg)
  
  val active_synaps_cnt_rst = RegEnable(io.fire, io.en)
  
  val busy = RegInit(false.B)
  when(io.fire) { busy := true.B }.elsewhen(done_shrg && accu_done) { busy := false.B }

  // control
  val current_synapse_active = Wire(Bool())
  current_synapse_active := sparcity_mem(io.k)
  val current_synapse_active_shrg = ShiftRegister(current_synapse_active, 5+MULT_LATENCY+sigmoid.LATENCY, io.en)

  val active_synaps_counter_next = RegInit(0.U(config.synapseCounterWidth.W))
  when (active_synaps_cnt_rst) {
    active_synaps_counter_next := 0.U
  }.elsewhen (current_synapse_active && io.en) {
    // inc counter, assuming overflow is impossible
    active_synaps_counter_next := active_synaps_counter_next + 1.U
  }
  // this ensures that the 1st enable yields zero
  val active_synaps_counter = RegEnable(active_synaps_counter_next, 0.U, current_synapse_active && io.en)

  // weigth address propagaion
  val mu_addr    = RegNext(active_synaps_counter)
  val gamma_addr = RegNext(mu_addr)
  val w_addr     = ShiftRegister(gamma_addr, 1+MULT_LATENCY+sigmoid.LATENCY)
  val Erev_addr  = ShiftRegister(w_addr, 1+(MULT_LATENCY-1))

  // datapath
  val x_in_shrg = ShiftRegister(io.x_z1, 3)
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

  when(accu_rst_shrg) {
    act_accu     := s_times_w_reg
    rev_act_accu := s_times_w_times_E_rev_reg
  }.otherwise {
    act_accu     := act_accu     + s_times_w_reg
    rev_act_accu := rev_act_accu + s_times_w_times_E_rev_reg
  }

  val done_out = RegInit(false.B)
  // when (done_shrg && accu_done) { done_out := true.B } // maybe accu_rst_shrg is better than accu_done
  when (done_shrg && (accu_rst_shrg || accu_done)) { done_out := true.B } // maybe accu_rst_shrg is better than accu_done
  .elsewhen(io.fire) { done_out := false.B }
  io.done := done_out
  io.busy := busy

  // output
  io.unit_out.valid := accu_rst_shrg && !done_out
  when(accu_rst_shrg) {
    io.unit_out.bits.act     := act_accu
    io.unit_out.bits.rev_act := rev_act_accu
  }

  if (config.DEBUG) {
    val out_missed_count = RegInit(0.U(config.xLen.W))
    when(io.unit_out.valid && !io.unit_out.ready) {
      out_missed_count := out_missed_count + 1.U
      printf("LTC Unit output was missed!!!! \n")
    }
    io.csr.unit_out_missed_count := out_missed_count
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
