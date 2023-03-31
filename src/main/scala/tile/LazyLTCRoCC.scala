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

class LTCUnit extends Module {
  val io = IO(new Bundle {
      val in = Input(UInt(16.W))
      val out = Output(UInt(16.W))
  })

  io.out := RegNext(io.in)
}