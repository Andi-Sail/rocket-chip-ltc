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

object LTCUnit_MemSel extends ChiselEnum {
  val mu, gamma, w, erev = Value
}

class LTCUnit_MemWrite(val w: Int, val addrWidth : Int) extends Bundle {
  val writeSelect = LTCUnit_MemSel()
  val writeAddr = Bits(addrWidth.W)
  val writeData = Bits(w.W)
}

class LTCUnit_SparcityMatWrite(val addrWidth : Int) extends Bundle {
  val writeAddr = Bits(addrWidth.W)
  val writeData = Bool()
}

class LTCUnit(  val w: Int = 32, val f: Int = 16, 
                val maxNeurons: Int = 256, 
                val ramBlockArrdWidth : Int = 9,
                val hwMultWidth : Int = 17
              ) extends Module {

  val neuronCounterWidth = log2Ceil(maxNeurons)
  val maxSynapses = pow(maxNeurons, 2).toInt
  val synapseCounterWidth = log2Ceil(maxSynapses)

  val io = IO(new Bundle {

      val en          = Input(Bool())
      val j           = Input(UInt(neuronCounterWidth.W)) 
      val last_state = Input(Bool())
      val x_z1        = Input(FixedPoint(w.W, f.BP))
      val k           = Input(UInt(synapseCounterWidth.W))
      val fire        = Input(Bool())

      val busy    = Output(Bool())
      val done    = Output(Bool())
      val act     = Output(FixedPoint(w.W, f.BP))
      val rev_act = Output(FixedPoint(w.W, f.BP))

      val valid   = Output(Bool())

      val N_out_neurons_write = Flipped(Valid(UInt(neuronCounterWidth.W)))
      val sparcity_write = Flipped(Valid(new LTCUnit_SparcityMatWrite(synapseCounterWidth)))
      val mem_write = Flipped(Valid(new LTCUnit_MemWrite(w, ramBlockArrdWidth))) // should be an input 😕

      val dummy_toggler_out = Output(Bool())
      val time_out = Output(UInt(32.W))
  })

  // component instantiation
  val sigmoid = Module(new HardSigmoid(w, f)) // TODO add sigmoid lut_addr_w --> maybe define LTC Proc config in general somewhere, like rocket config

  // TODO: add multiplier latency based on W. --> It needs more if W == 32
  // constants
  // val MULT_LATENCY = if (w > 17) {4} else {1}
  val MULT_LATENCY = 1
  println(s"Mult lantency is: $MULT_LATENCY")
  val LATENCY = sigmoid.LATENCY + 8 + 2*MULT_LATENCY + (MULT_LATENCY-1) // Latency input to output
  val THROUGHPUT = 1 // 1 synapse per cc

  // TODO: should be removed in the end! Why is this still necessarry?
  // default assigment to remove errors during development 
  io <> DontCare
  sigmoid.io <> DontCare
  val dummy_toggler = RegInit(false.B)
  dummy_toggler := !dummy_toggler
  io.dummy_toggler_out := dummy_toggler

  val time_reg = RegInit(0.U(32.W))
  when (io.en) {
    time_reg := time_reg + 1.U
  }
  io.time_out := time_reg


  val N_out_neurons = RegEnable(io.N_out_neurons_write.bits, 0.U, io.N_out_neurons_write.valid)

  // memory definition
  var weight_mems : Map[LTCUnit_MemSel.Type, SyncReadMem[FixedPoint] ] = Map()
  LTCUnit_MemSel.all.foreach{
    m => weight_mems += (m -> SyncReadMem(pow(2, ramBlockArrdWidth).toInt, FixedPoint(w.W, f.BP)))
  }
  val sparcity_mem = SyncReadMem(maxSynapses, Bool())

  // memory write 
  when(io.mem_write.fire) {
    LTCUnit_MemSel.all.foreach{
    m => {
      when(io.mem_write.bits.writeSelect === m) {
        weight_mems(m)(io.mem_write.bits.writeAddr) := io.mem_write.bits.writeData.asFixedPoint(f.BP)
        }
      }
    }
  }
  when(io.sparcity_write.fire) {
    sparcity_mem(io.sparcity_write.bits.writeAddr) := io.sparcity_write.bits.writeData
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

  val active_synaps_counter_next = RegInit(0.U(synapseCounterWidth.W))
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
  val mu = weight_mems(LTCUnit_MemSel.mu)(mu_addr)
  val x_minus_mu = RegNext(x_in_shrg - mu)
  val gamma = weight_mems(LTCUnit_MemSel.gamma)(gamma_addr)
  val sigmoid_in = Wire(FixedPoint(w.W, f.BP))
  sigmoid_in := gamma * x_minus_mu
  sigmoid.io.x := ShiftRegister(sigmoid_in, MULT_LATENCY)
  val sigmoid_out = RegNext(sigmoid.io.y)
  val w_weight = weight_mems(LTCUnit_MemSel.w)(w_addr)
  val s_times_w_1 = Wire(FixedPoint(w.W, f.BP))
  s_times_w_1 := sigmoid_out * w_weight
  val s_times_w = ShiftRegister(Mux(
      current_synapse_active_shrg,
        s_times_w_1, 
        0.U.asFixedPoint(f.BP)
    ), MULT_LATENCY)
  val E_rev = weight_mems(LTCUnit_MemSel.erev)(Erev_addr)

  // activation accumulators
  val act_accu = Reg(chiselTypeOf(io.act))
  val rev_act_accu = Reg(chiselTypeOf(io.rev_act))

  val s_times_w_times_E_rev = Wire(FixedPoint(w.W, f.BP))
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

  // output
  io.valid := accu_rst_shrg
  when(accu_rst_shrg) {
    io.act     := act_accu
    io.rev_act := rev_act_accu
  }

  io.done := done_shrg && accu_done
  io.busy := busy

}

class HardSigmoid(val w: Int = 32, val f: Int = 16, lut_addr_w: Int = 6) extends Module {
  val io = IO(new Bundle {
    val x = Input(FixedPoint(w.W, f.BP))
    val y = Output(FixedPoint(w.W, f.BP))
  })

  val LATENCY : Int = 2
  val lut_len : Int = pow(2, lut_addr_w).toInt
  val step_size : Double = 8.0/lut_len

  require(lut_len >= 8, s"HardSigmoid lut_len must be >= 8 - lut_len is $lut_len with lut_addr_w $lut_addr_w")

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
  val error_lut_quant = error_lut_d.map(x => round(x*pow(2.0d,f)).toInt)
  val max_entry = blinag.max(blinag.DenseVector[Int](error_lut_quant))
  val rom_bit_w = log2Ceil(max_entry)
  val rom_lut_memory_bits = rom_bit_w * lut_len
  println(s"using $rom_bit_w bits for $lut_len entries in Sigmoid Error LUT --> LUT requires $rom_lut_memory_bits bits")
  println(s"Correcting Error in the range from $x_min_d to $x_max_d")

  val lut_x_shift = lut_addr_w-3 // b.c. fixed x_max of 8
  val rom_lut = VecInit(error_lut_quant.map(_.U(rom_bit_w.W)))

  val x_sign = (io.x > FixedPoint(0, w.W, f.BP))
  val x_sign_shrg = ShiftRegister(x_sign, 2, true.B)

  // linear interpolation
  val y_interpolated_raw = (io.x >> 2) + FixedPoint.fromDouble(0.5, w.W, f.BP)
  val y_interpolated = MuxCase(
    y_interpolated_raw, // default
    Array(
      (io.x < FixedPoint.fromDouble(-2.0, w.W, f.BP)) -> FixedPoint.fromDouble(0.0, w.W, f.BP),
      (io.x > FixedPoint.fromDouble(+2.0, w.W, f.BP)) -> FixedPoint.fromDouble(1.0, w.W, f.BP)
      )
    )
  val y_interpolated_shrg = ShiftRegister(y_interpolated, 2, true.B)

  // error correction
  val x_abs = io.x.abs()

  def getErrorLutAddr(x : FixedPoint ) : UInt = {
    val addr = (x.asUInt >> (f - lut_x_shift).asUInt) + ((x.asUInt)(f - lut_x_shift -1))
    Mux((x >= FixedPoint.fromDouble(8.0, w.W, f.BP)), 0.U, addr)
  }

  val addr = Reg(UInt(lut_addr_w.W))
  addr := getErrorLutAddr(x_abs)
  val e_reg = Reg(FixedPoint(w.W, f.BP))
  val e = Wire(UInt(w.W))
  e := rom_lut(addr)
  e_reg := e.asFixedPoint(f.BP) 
  // printf(cf"addr = $addr - e = $e \n")

  // output
  io.y := Mux(
        x_sign_shrg, 
          y_interpolated_shrg - e_reg, 
          y_interpolated_shrg + e_reg
      )
}
