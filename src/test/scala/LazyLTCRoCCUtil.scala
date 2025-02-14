
import chisel3._
import chiseltest._

import freechips.rocketchip.tile.{LTCPE_WeightSel, LTCPE_MemoryWriteIF, LTCCoprocConfig}

import scala.math._

// JSON reader
import io.circe._
import io.circe.parser._
import chisel3.util.Valid
import freechips.rocketchip.tile.LTCCore_StateWrite
import freechips.rocketchip.tile.LTCPE_CSRs_IO
import freechips.rocketchip.tile.LTCPE_CSRs
import freechips.rocketchip.tile.LTCCore_CSRs_IO
import freechips.rocketchip.tile.LTCCore_CSRs

/**
  * Utility functions to read test data
  */
object LTCTestDataUtil {

  /**
    * Reads an LTC Model definition and corresponding weights from a header file 
    * (as it is used in C Implementation)
    *
    * @param headerFilePaht path to the header file
    * @param F F of the fix-point format used
    * @return touple holding values read
    */
  def ReadLTCModelFromHeader(headerFilePaht : String, F : Int): 
    (Int, Int, Map[LTCPE_WeightSel.Type, List[Int]], Array[List[Int]], 
     Int, Int, Map[LTCPE_WeightSel.Type, List[Int]], Array[List[Int]]) = {
    // load model definition from C-header file ("if it works it ain't stupit 🙃" - ChatGPT (probably))
    val cHeaderString: String = scala.io.Source.fromFile(headerFilePaht).mkString

    val n_neurons = """(\d+)""".r.findFirstIn("""#define units (\d+)""".r.findFirstIn(cHeaderString).get).get.toInt
    val ode_synapses = """(\d+)""".r.findFirstIn("""#define ode_synapses (\d+)""".r.findFirstIn(cHeaderString).get).get.toInt
    val rnn_ltc_cell_sigma_0_sparse = s"fix${F}_t rnn_ltc_cell_sigma_0_sparse\\[${ode_synapses}\\] = \\{(.*?)\\};".r.findFirstMatchIn(cHeaderString).map{m => m.group(1)}.get.split(',').map(_.trim.toInt).toList
    val rnn_ltc_cell_mu_0_sparse    = s"fix${F}_t rnn_ltc_cell_mu_0_sparse\\[${ode_synapses}\\] = \\{(.*?)\\};".r.findFirstMatchIn(cHeaderString).map{m => m.group(1)}.get.split(',').map(_.trim.toInt).toList
    val rnn_ltc_cell_w_0_sparse     = s"fix${F}_t rnn_ltc_cell_w_0_sparse\\[${ode_synapses}\\] = \\{(.*?)\\};".r.findFirstMatchIn(cHeaderString).map{m => m.group(1)}.get.split(',').map(_.trim.toInt).toList
    val rnn_ltc_cell_erev_0_sparse  = s"fix${F}_t rnn_ltc_cell_erev_0_sparse\\[${ode_synapses}\\] = \\{(.*?)\\};".r.findFirstMatchIn(cHeaderString).map{m => m.group(1)}.get.split(',').map(_.trim.toInt).toList
    val weigth_map = Map(
      LTCPE_WeightSel.gamma -> rnn_ltc_cell_sigma_0_sparse,
      LTCPE_WeightSel.mu    -> rnn_ltc_cell_mu_0_sparse,
      LTCPE_WeightSel.w     -> rnn_ltc_cell_w_0_sparse,
      LTCPE_WeightSel.erev  -> rnn_ltc_cell_erev_0_sparse,
    )
    // NOTE: sparcity_matrix is not transposed!!!!
    val sparcity_matrix = s"adjacency_matrix\\[${n_neurons}\\]\\[${n_neurons}\\] = \\{(.*?)\\};".r.findFirstMatchIn(cHeaderString)
    .map{m => m.group(1)}.get.split("""\},\{""")
    .map{s => s.replace("""{""", "").replace("""}""", "").split(',').map(x => abs(x.trim.toInt)).toList}

    val sensory_n_neurons = """(\d+)""".r.findFirstIn("""#define input_dim (\d+)""".r.findFirstIn(cHeaderString).get).get.toInt
    val sensory_synapses = """(\d+)""".r.findFirstIn("""#define sensory_synapses (\d+)""".r.findFirstIn(cHeaderString).get).get.toInt
    val sensory_rnn_ltc_cell_sigma_0_sparse = s"fix${F}_t rnn_ltc_cell_sensory_sigma_0_sparse\\[${sensory_synapses}\\] = \\{(.*?)\\};".r.findFirstMatchIn(cHeaderString).map{m => m.group(1)}.get.split(',').map(_.trim.toInt).toList
    val sensory_rnn_ltc_cell_mu_0_sparse    = s"fix${F}_t rnn_ltc_cell_sensory_mu_0_sparse\\[${sensory_synapses}\\] = \\{(.*?)\\};".r.findFirstMatchIn(cHeaderString).map{m => m.group(1)}.get.split(',').map(_.trim.toInt).toList
    val sensory_rnn_ltc_cell_w_0_sparse     = s"fix${F}_t rnn_ltc_cell_sensory_w_0_sparse\\[${sensory_synapses}\\] = \\{(.*?)\\};".r.findFirstMatchIn(cHeaderString).map{m => m.group(1)}.get.split(',').map(_.trim.toInt).toList
    val sensory_rnn_ltc_cell_erev_0_sparse  = s"fix${F}_t rnn_ltc_cell_sensory_erev_0_sparse\\[${sensory_synapses}\\] = \\{(.*?)\\};".r.findFirstMatchIn(cHeaderString).map{m => m.group(1)}.get.split(',').map(_.trim.toInt).toList
    val sensory_weigth_map = Map(
      LTCPE_WeightSel.gamma -> sensory_rnn_ltc_cell_sigma_0_sparse,
      LTCPE_WeightSel.mu    -> sensory_rnn_ltc_cell_mu_0_sparse,
      LTCPE_WeightSel.w     -> sensory_rnn_ltc_cell_w_0_sparse,
      LTCPE_WeightSel.erev  -> sensory_rnn_ltc_cell_erev_0_sparse,
    )
    // NOTE: sparcity_matrix is not transposed!!!!
    val sensory_sparcity_matrix = s"adjacency_matrix\\[${sensory_n_neurons}\\]\\[${n_neurons}\\] = \\{(.*?)\\};".r.findFirstMatchIn(cHeaderString)
    .map{m => m.group(1)}.get.split("""\},\{""")
    .map{s => s.replace("""{""", "").replace("""}""", "").split(',').map(x => abs(x.trim.toInt)).toList}

    return (
      n_neurons, ode_synapses, weigth_map, sparcity_matrix, 
      sensory_n_neurons, sensory_synapses, sensory_weigth_map, sensory_sparcity_matrix
    )
  }

  /**
    * Reads the stimuli and expected responses from a JSON file
    * (as generated from C Implementation)
    * Those values are the inputs and outputs from the RoCC accelerator
    *
    * @param jsonFilePath patht to the json file
    * @return A list holding the a map of values for each iteration
    */
  def LoadLTCRoCCStimuli(jsonFilePath : String) : Array[Map[String,Array[Int]]] = {
    // load rocc input and output values from json
    val jsonString = scala.io.Source.fromFile(jsonFilePath).mkString
    val json = parse(jsonString).getOrElse(Json.Null)
    val ltc_rocc_values = json.as[Array[Map[String, Array[Int]]]].toSeq(0)

    return ltc_rocc_values
  }
}

/**
  * Utility functions for common functions in LTC unit tests
  */
object LTCTestUtil {

  /**
    * Writes a value to a specific PE CSR
    *
    * @param io_csr the PE CSR io interface
    * @param clock the dut clock
    * @param csr_id the ID of the CSR to write
    * @param csr_value the value to write to the CSR
    */
  def WritePE_CSR(io_csr : LTCPE_CSRs_IO, clock : Clock, csr_id: LTCPE_CSRs.Type, csr_value : UInt) {
      io_csr.csrWrite.bits.poke(csr_value)
      io_csr.csrWrite.valid.poke(true)
      io_csr.csrSel.bits.poke(csr_id)
      io_csr.csrSel.valid.poke(true)
      clock.step()
      io_csr.csrSel.valid.poke(false)
      io_csr.csrWrite.valid.poke(false)
      clock.step()
  }

    /**
    * Writes a value to a specific Core CSR
    *
    * @param io_csr the Core CSR io interface
    * @param clock the dut clock
    * @param csr_id the ID of the CSR to write
    * @param csr_value the value to write to the CSR
    */
  def WriteCore_CSR(io_csr : LTCCore_CSRs_IO, clock : Clock, csr_id: LTCCore_CSRs.Type, csr_value : UInt) {
      io_csr.csrWrite.bits.poke(csr_value)
      io_csr.csrWrite.valid.poke(true)
      io_csr.csrSel.bits.poke(csr_id)
      io_csr.csrSel.valid.poke(true)
      clock.step()
      io_csr.csrSel.valid.poke(false)
      io_csr.csrWrite.valid.poke(false)
      clock.step()
  }

  /**
    * Write (i.e. poke) LTC weights and model data to LTC PE
    *
    * @param mem_if memory interface of LTC PE
    * @param clock clock of LTC PE
    * @param config coprocessor config
    * @param N_in_neurons model param - number of input neurons for this pe (usually total number of neurons)
    * @param N_out_neurons number of output neurons for this pe (must be <= N_in_neurons)
    * @param weigth_map model weights
    * @param sparcity_matrix model sparcity matrix
    */
  def WriteModelData2PE(csr : LTCPE_CSRs_IO, mem_if : LTCPE_MemoryWriteIF, clock : Clock, config : LTCCoprocConfig,
  N_in_neurons : Int, N_out_neurons : Int, weigth_map : Map[LTCPE_WeightSel.Type, List[Int]], sparcity_matrix : Array[List[Int]]) : Int = {

        val out_neurons_fanint = Array.fill(N_out_neurons)(0)
        var total_active_synapses = 0
        // write sparcity
        println("writing someting to sparcity")
        mem_if.sparcity_write.valid.poke(true.B)
        for (j <- 0 until N_out_neurons)
        {
          for (i <- 0 until N_in_neurons)
          {
            val addr = j*N_in_neurons + i
            mem_if.sparcity_write.bits.writeAddr.poke(addr)
            mem_if.sparcity_write.bits.writeData.poke(sparcity_matrix(i)(j)) // NOTE: mat is transposed here!!!!!!!!!
            out_neurons_fanint(j) += sparcity_matrix(i)(j)
            total_active_synapses += sparcity_matrix(i)(j)
            clock.step()
          }
        }
        mem_if.sparcity_write.valid.poke(false)

        println("out_neurons_fanin")
        println(out_neurons_fanint.toList)
        clock.step()

        // write memories
        mem_if.weight_write.valid.poke(true)
        LTCPE_WeightSel.all.foreach{ m =>
          println(s"writing something to $m")
          mem_if.weight_write.bits.writeSelect.poke(m)
          for (i <- 0 until total_active_synapses)
          {
            mem_if.weight_write.bits.writeAddr.poke(i)
            mem_if.weight_write.bits.writeData.poke(weigth_map(m)(i).asSInt(config.w.W)) 
            clock.step()
          }
        }
        mem_if.weight_write.valid.poke(false)

        return total_active_synapses
  } 

  def WriteStates2Core(mem_if : Valid[LTCCore_StateWrite],  clock : Clock, config : LTCCoprocConfig,
                       states : Array[Int]) = {
    
    mem_if.valid.poke(true)
    for (i <- 0 until states.length) {
      mem_if.bits.stateAddr.poke(i)
      mem_if.bits.stateValue.poke(states(i))
      clock.step()
    }
    mem_if.valid.poke(false)
  }
}