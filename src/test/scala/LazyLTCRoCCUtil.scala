
import chisel3._
import chiseltest._

import freechips.rocketchip.tile.{LTCUnit_WeightSel, LTCUnit_MemoryWriteIF, LTCCoprocConfig}

import scala.math._

// JSON reader
import io.circe._
import io.circe.parser._

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
    (Int, Int, Map[LTCUnit_WeightSel.Type, List[Int]], Array[List[Int]]) = {
    // load model definition from C-header file ("if it works it ain't stupit 🙃" - ChatGPT (probably))
    val cHeaderString: String = scala.io.Source.fromFile(headerFilePaht).mkString
    val units = """(\d+)""".r.findFirstIn("""#define units (\d+)""".r.findFirstIn(cHeaderString).get).get.toInt
    val ode_synapses = """(\d+)""".r.findFirstIn("""#define ode_synapses (\d+)""".r.findFirstIn(cHeaderString).get).get.toInt
    val rnn_ltc_cell_sigma_0_sparse = s"fix${F}_t rnn_ltc_cell_sigma_0_sparse\\[${ode_synapses}\\] = \\{(.*?)\\};".r.findFirstMatchIn(cHeaderString).map{m => m.group(1)}.get.split(',').map(_.trim.toInt).toList
    val rnn_ltc_cell_mu_0_sparse    = s"fix${F}_t rnn_ltc_cell_mu_0_sparse\\[${ode_synapses}\\] = \\{(.*?)\\};".r.findFirstMatchIn(cHeaderString).map{m => m.group(1)}.get.split(',').map(_.trim.toInt).toList
    val rnn_ltc_cell_w_0_sparse     = s"fix${F}_t rnn_ltc_cell_w_0_sparse\\[${ode_synapses}\\] = \\{(.*?)\\};".r.findFirstMatchIn(cHeaderString).map{m => m.group(1)}.get.split(',').map(_.trim.toInt).toList
    val rnn_ltc_cell_erev_0_sparse  = s"fix${F}_t rnn_ltc_cell_erev_0_sparse\\[${ode_synapses}\\] = \\{(.*?)\\};".r.findFirstMatchIn(cHeaderString).map{m => m.group(1)}.get.split(',').map(_.trim.toInt).toList
    val weigth_map = Map(
      LTCUnit_WeightSel.gamma -> rnn_ltc_cell_sigma_0_sparse,
      LTCUnit_WeightSel.mu    -> rnn_ltc_cell_mu_0_sparse,
      LTCUnit_WeightSel.w     -> rnn_ltc_cell_w_0_sparse,
      LTCUnit_WeightSel.erev  -> rnn_ltc_cell_erev_0_sparse,
    )
    // NOTE: sparcity_matrix is not transposed!!!!
    val sparcity_matrix = s"int adjacency_matrix\\[${units}\\]\\[${units}\\] = \\{(.*?)\\};".r.findFirstMatchIn(cHeaderString)
    .map{m => m.group(1)}.get.split("""\},\{""")
    .map{s => s.replace("""{""", "").replace("""}""", "").split(',').map(x => abs(x.trim.toInt)).toList}

    return (units, ode_synapses, weigth_map, sparcity_matrix)
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
  * Utility functions for common functions in LTC Unit Tests
  */
object LTCTestUtil {

  /**
    * Write (i.e. poke) LTC weights and model data to LTC Unit
    *
    * @param mem_if memory interface of LTC Unit
    * @param clock clock of LTC Unit
    * @param config coprocessor config
    * @param units model param
    * @param ode_synapses model param
    * @param weigth_map model weights
    * @param sparcity_matrix model sparcity matrix
    */
  def WriteModelData2Unit(mem_if : LTCUnit_MemoryWriteIF, clock : Clock, config : LTCCoprocConfig,
  units : Int, ode_synapses : Int, weigth_map : Map[LTCUnit_WeightSel.Type, List[Int]], sparcity_matrix : Array[List[Int]]) = {

        // write N out neurons
        mem_if.N_out_neurons_write.bits.poke(units)
        mem_if.N_out_neurons_write.valid.poke(true)
        clock.step()
        mem_if.N_out_neurons_write.valid.poke(false)
        clock.step()

        // write sparcity
        println("writing someting to sparcity")
        mem_if.sparcity_write.valid.poke(true.B)
        for (i <- 0 until units)
        {
          for (j <- 0 until units)
          {
            val addr = i*units + j
            mem_if.sparcity_write.bits.writeAddr.poke(addr)
            mem_if.sparcity_write.bits.writeData.poke(sparcity_matrix(j)(i)) // NOTE: mat is transposed here!!!!!!!!!
            clock.step()
          }
        }
        mem_if.sparcity_write.valid.poke(false)

        clock.step()

        // write memories
        mem_if.weight_write.valid.poke(true)
        LTCUnit_WeightSel.all.foreach{ m =>
          println(s"writing something to $m")
          mem_if.weight_write.bits.writeSelect.poke(m)
          for (i <- 0 until ode_synapses)
          {
            mem_if.weight_write.bits.writeAddr.poke(i)
            mem_if.weight_write.bits.writeData.poke(weigth_map(m)(i).asSInt(config.w.W)) 
            clock.step()
          }
        }
        mem_if.weight_write.valid.poke(false)
  } 
}