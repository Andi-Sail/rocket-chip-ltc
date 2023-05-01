
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chisel3.stage.PrintFullStackTraceAnnotation
import org.scalatest.funsuite.AnyFunSuite

import freechips.rocketchip.tile.{LTCUnit, HardSigmoid, LTCUnit_WeightSel, LTCCore}
import chisel3.experimental.FixedPoint

import  Array._
import scala.math._
import breeze.linalg.trace

// import breeze.linalg._
import breeze.{linalg => blinag}
// import breeze.numerics._
import breeze.{numerics => bnumerics}

// JSON reader
import io.circe._
import io.circe.parser._
import freechips.rocketchip.tile.LTCCoprocConfig


// sigmoid test independed of HW implementation
class SigmoidLUTTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "HardSigmoid" 
  it should "generate a correct fix16 error LUT" in {
    test(
      new HardSigmoid(new LTCCoprocConfig(w=32, f=16, sigmoid_lut_addr_w=6))
      ) { c =>
        // expected LUTs generated with: python3 GenerateSigmoidErrorLut.py -la 6
        println("Checking float LUT")
        val exp_lut_f = Array(-0.0,4.0626626243711605e-05,0.00032349911420193056,0.0010834000459303272,0.002540668798145407,0.00489513533394581,0.008321300824607025,0.012964972162988753,0.018941421369995104,0.026335013132371743,0.035200138825308924,0.04556322226037879,0.05742552380635635,0.07076646289656308,0.08554719803168942,0.10171424019782938,0.11920292202211769,0.10669059394565128,0.09534946489910945,0.08509904500702026,0.07585818002124345,0.06754669113962908,0.060086650174007605,0.05340332979982432,0.047425873177566635,0.04208772791561888,0.03732688734412937,0.033085978388704196,0.02931223075135636,0.025957357197796904,0.022977369910025636,0.020332353342658815,0.01798620996209155,0.015906391711814738,0.014063627043245597,0.012431650853185872,0.010986942630593188,0.009708476481474104,0.00857748541371195,0.007577241267860746,0.006692850924284732,0.005911068856243817,0.005220125693558342,0.004609572179374077,0.004070137715896038,0.0035936025814201633,0.003172682842485175,0.002800926967121087,0.002472623156634657,0.0021827164453452896,0.0019267346633274895,0.0017007224114351516,0.0015011822567370103,0.0013250224172132175,0.0011695102650555178,0.0010322310367547605,0.0009110511944006028,0.0008040859356291952,0.000709670399100526,0.0006263341581095316,0.000552778636923601,0.0004878571227822093,0.0004305570813245563,0.00037998451475196315)
        val act_lut_f = c.error_lut_d
        for ( i <- 0 until 64)
        {
          assert(exp_lut_f(i) == act_lut_f(i), s"error float lut missmatch $i")
        }

        val exp_lut_fix = Array(0,3,21,71,167,321,545,850,1241,1726,2307,2986,3763,4638,5606,6666,7812,6992,6249,5577,4971,4427,3938,3500,3108,2758,2446,2168,1921,1701,1506,1333,1179,1042,922,815,720,636,562,497,439,387,342,302,267,236,208,184,162,143,126,111,98,87,77,68,60,53,47,41,36,32,28,25)
        val act_lut_fix = c.error_lut_quant
        println("Checking fix16 LUT")
        for ( i <- 0 until 64)
        {
          // round to four digits, due to double <-> float difference
          assert(exp_lut_fix(i) == act_lut_fix(i), s"error fix lut missmatch $i")
        }
    }
  }
  it should "generate a correct fix8 error LUT" in {
    test(
      new HardSigmoid(new LTCCoprocConfig(w=16, f=8, sigmoid_lut_addr_w=7))
      ) { c =>
        // expected LUTs generated with: python3 GenerateSigmoidErrorLut.py -la 7
        println("Checking float LUT")
        val exp_lut_f = Array(-0.0,5.08427698442393e-06,4.0626626243711605e-05,0.00013684801538615954,0.00032349911420193056,0.0006296348141882069,0.0010834000459303272,0.001711830167108297,0.002540668798145407,0.0035942055196168665,0.00489513533394581,0.006464441253186015,0.008321300824607025,0.010483016865263872,0.012964972162988753,0.01578060742914389,0.018941421369995104,0.022456991387518888,0.026335013132371743,0.030581356914040292,0.035200138825308924,0.04019380435710529,0.04556322226037879,0.05130778644723677,0.05742552380635635,0.06391320592932659,0.07076646289656308,0.07797989744545741,0.08554719803168942,0.09346124949008328,0.10171424019782938,0.11029776483513232,0.11920292202211769,0.1127954062828932,0.10669059394565128,0.10087862273005643,0.09534946489910945,0.0900929939619518,0.08509904500702026,0.08035746882220707,0.07585818002124345,0.07159119944455239,0.06754669113962908,0.06371499425196536,0.060086650174007605,0.056652425307973875,0.05340332979982432,0.05033063259747683,0.047425873177566635,0.04468087027265022,0.04208772791561888,0.03963883910096999,0.03732688734412937,0.035144846400793295,0.033085978388704196,0.031143830534778427,0.02931223075135636,0.02758528222679013,0.025957357197796904,0.024423090054107144,0.022977369910025636,0.0216153327626476,0.020332353342658815,0.019124036750888807,0.01798620996209155,0.01691491326672656,0.015906391711814738,0.014957086593149982,0.014063627043245597,0.013222821752305158,0.012431650853185872,0.011687257995694367,0.010986942630593188,0.010328152519305078,0.009708476481474104,0.009125637389180596,0.00857748541371195,0.008061991528271584,0.007577241267860746,0.00712142874573507,0.006692850924284732,0.006289902136892511,0.005911068856243817,0.005554924703691011,0.005220125693558342,0.004905405705722066,0.004609572179374077,0.004331502020563871,0.004070137715896038,0.0038244836446372776,0.0035936025814201633,0.0033766123817393634,0.003172682842485175,0.002981032729854838,0.002800926967121087,0.0026316739748845075,0.002472623156634657,0.002323162522631206,0.0021827164453452896,0.00205074353991741,0.0019267346633274895,0.0018102110262026017,0.0017007224114351516,0.0015978454940173137,0.0015011822567370103,0.0014103584966180804,0.0013250224172132175,0.0012448433020884053,0.0011695102650555178,0.0010987310729246857,0.0010322310367547605,0.000969751967782595,0.0009110511944006028,0.0008559006367451216,0.0008040859356291952,0.0007554056327294667,0.000709670399100526,0.0006667023092434832,0.0006263341581095316,0.0005884088185569292,0.000552778636923601,0.0005193048644950293,0.0004878571227822093,0.0004583129006350273,0.0004305570813245563,0.00040448149784211296,0.00037998451475196315,0.0003569706350377011)
        val act_lut_f = c.error_lut_d
        for ( i <- 0 until 64)
        {
          assert(exp_lut_f(i) == act_lut_f(i), s"error float lut missmatch $i")
        }

        val exp_lut_fix = Array(0,0,0,0,0,0,0,0,1,1,1,2,2,3,3,4,5,6,7,8,9,10,12,13,15,16,18,20,22,24,26,28,31,29,27,26,24,23,22,21,19,18,17,16,15,15,14,13,12,11,11,10,10,9,8,8,8,7,7,6,6,6,5,5,5,4,4,4,4,3,3,3,3,3,2,2,2,2,2,2,2,2,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
        val act_lut_fix = c.error_lut_quant
        println("Checking fix8 LUT")
        for ( i <- 0 until 64)
        {
          // round to four digits, due to double <-> float difference
          assert(exp_lut_fix(i) == act_lut_fix(i), s"error fix lut missmatch $i")
        }
    }
  }
}

class SigmoidImplTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "HardSigmoid" 

  val stims = Array(
    // fix8 stimuli and expected generated with python3 -i GenerateSigmoidErrorLut.py -la 7 (and exporting stim8 and exp8)
    Array(-539,-1720,784,935,-400,207,-302,-422,-1179,753,683,325,-616,-2499,1171,12,-150,-248,272,-119,-911,340,-717,1739,-1400,-1911,-1530,-761,1129,-76,979,-520,315,-13,25,162,298,-683,-1131,-561,384,-101,-534,820,-532,741,536,671,1550,198,219,1745,-378,908,587,901,376,1484,11,-734,-527,767,238,611,66,316,-1940,508,-236,-87,1147,429,-214,1001,-315,683,469,-1187,-676,365,-1355,1351,-508,-140,-325,-516,-1223,807,155,552,860,-369,1376,564,-98,-688,430,408,-1679,-239),
    // fix16 stimuli and expected generated with python3 -i GenerateSigmoidErrorLut.py -la 6 (and exporting stim16 and exp16)
    Array(39738,-213402,238796,231750,168498,93508,261338,-29564,62583,-180342,-6678,-158233,-131515,411298,-229534,-46142,-44557,47794,-208491,15337,427985,229908,-214443,-34821,-155064,-171932,196854,-332304,150035,-248300,-119194,202658,-63730,144727,-23916,-590807,-287550,-55807,-169720,-52184,76898,32728,251458,-432450,48529,-69367,-190420,-208039,-1686,234628,-142221,158842,144539,-210985,124011,6807,60530,22420,249368,590686,114904,204085,51106,-14010,102236,-65699,-56394,75922,-120045,35145,103237,62613,-45133,174036,-242747,-26160,39384,-155967,317395,-26930,31747,209489,-534222,-232475,169520,-243268,-154981,-131543,388610,-367911,-248821,407257,98385,-28296,107575,-397886,-272620,-384953,-252959,250983)
  )
  val exps =  Array(
    Array(27,0,245,249,44,176,60,40,2,243,240,200,21,0,253,131,91,71,190,98,7,203,15,256,1,0,1,12,253,109,250,29,197,124,134,167,194,16,3,26,209,102,29,246,29,242,229,239,255,175,179,256,48,249,233,248,207,255,130,14,29,244,183,234,144,198,0,224,73,106,253,215,77,251,58,240,221,2,17,206,1,255,32,94,55,31,2,245,165,230,248,48,255,230,103,16,215,212,0,72),
    Array(42381,2446,63835,63615,61109,53159,64357,25544,47172,3938,31101,5577,7812,65410,1921,21777,21949,44171,2758,36581,65438,63615,2446,24229,5577,4427,62428,387,59287,1506,9635,62778,18076,59287,26860,0,815,19666,4427,20267,50266,40783,64203,87,44355,16667,3500,2758,32346,63835,6992,59959,59287,2446,57104,34466,47050,38302,64030,65536,55888,62778,44999,29286,54564,17584,19519,50022,9422,41387,53939,47180,22029,61109,1506,26299,42293,5577,65039,26106,40537,63090,0,1921,61109,1506,5577,7812,65352,236,1506,65410,53601,25765,55023,143,1042,184,1333,64203)
  )

  val fix_configs = Array(8,16)
  val lut_addr_w_configs = Array(7,6)

  val test_len = 100

  for (config_index <- 0 until 2)
  {
    val fix = fix_configs(config_index)
    val lut_addr_config = lut_addr_w_configs(config_index)
    it should s"compute hard sigmoid with fix$fix - config $config_index" in {
      val W = fix*2
      val F = fix
      test(
        new HardSigmoid(new LTCCoprocConfig(w=W, f=F, sigmoid_lut_addr_w=lut_addr_config)), 
        ).withAnnotations(Seq(
          WriteVcdAnnotation, 
          treadle.WriteCoverageCSVAnnotation,
          )) { c =>

          val stim = stims(config_index)
          val exp = exps(config_index)

          var act_in  = FixedPoint(0, W.W, F.BP)
          var exp_out = FixedPoint(0, W.W, F.BP)
          var act_out = FixedPoint(0, W.W, F.BP)
  
          // assuming 2 cc latency: x -> y
          c.clock.step() // step reset
          c.io.x.poke(FixedPoint(stim(0), W.W, F.BP))
          c.clock.step()
          c.io.x.poke(FixedPoint(stim(1), W.W, F.BP))
          c.clock.step()
          for ( i <- 2 until test_len)
          {
            act_in  = FixedPoint(stim(i-2), W.W, F.BP)
            exp_out = FixedPoint(exp(i-2), W.W, F.BP)
            act_out = c.io.y.peek()
            // println(s"cycle: $i - Exp: $exp_out - Act: $act_out - In: $act_in")
            c.io.y.expect(FixedPoint(exp(i-2), W.W, F.BP), s"missmach at index: $i")
            c.io.x.poke(FixedPoint(stim(i), W.W, F.BP))
            c.clock.step()
          }
          act_in  = FixedPoint(stim(test_len-2), W.W, F.BP)
          exp_out = FixedPoint(exp(test_len-2), W.W, F.BP)
          act_out = c.io.y.peek()
          // println(s"cycle: last-1 - Exp: $exp_out - Act: $act_out - In: $act_in")
          c.io.y.expect(FixedPoint(exp(test_len-2), W.W, F.BP), s"missmach at index: last-1")
          c.clock.step()
          act_in  = FixedPoint(stim(test_len-1), W.W, F.BP)
          exp_out = FixedPoint(exp(test_len-1), W.W, F.BP)
          act_out = c.io.y.peek()
          // println(s"cycle: last - Exp: $exp_out - Act: $act_out - In: $act_in")
          c.io.y.expect(FixedPoint(exp(test_len-1), W.W, F.BP), s"missmach at index: last")
      }
    }
  }
}


class LTCUnit_Latency_test extends AnyFlatSpec with ChiselScalatestTester {
  
  behavior of "LTCUnit"
  // test class body here
  for (w <- Seq(16,32))
  {
    it should s"check the latency of the LTC Unit with w=$w" in {
      test(new LTCUnit(new LTCCoprocConfig(w=w, f=w/2))).withAnnotations(Seq(PrintFullStackTraceAnnotation)) { c =>
        val exp_mult_latency = if (w > 17) {4} else {1}
        val exp_latency = 2 + 8 + 2*exp_mult_latency + (exp_mult_latency-1)
        assert(c.LATENCY == exp_latency)
      }
    }
  }
}

class LTCUnit_Datapath_test extends AnyFlatSpec with ChiselScalatestTester {
  
  behavior of "LTCUnit"
  // test class body here
  it should "activate done according N_out_neurons " in {
    val N_out_neurons_test = 14
    test(new LTCUnit(new LTCCoprocConfig())).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      c.clock.step() // step reset
      c.io.en.poke(false.B) // disable LTC Unit
      c.io.j.poke(0.U)
      
      // write some value to N_out_neurons_write
      c.io.memWrite.N_out_neurons_write.bits.poke(N_out_neurons_test.U)
      c.io.memWrite.N_out_neurons_write.valid.poke(true.B)
      
      c.clock.step()
      c.io.en.poke(true.B)
      c.io.unit_out.ready.poke(true) // we do not really care about data out here anyway

      c.io.memWrite.N_out_neurons_write.bits.poke(42.U)
      c.io.memWrite.N_out_neurons_write.valid.poke(false.B)
      // check done is low
      c.io.done.expect(false.B, s"done is set before anything happens")
      
      // step and increment j until j > N_out_neurons
      for (j <- 0 until N_out_neurons_test)
      {
        c.io.done.expect(false.B, s"done is set before j reaches max $N_out_neurons_test - j is only at $j")
        c.clock.step()
        c.io.j.poke(j.U)
      }
      c.io.last_state.poke(true)
      // check done is low until 9+2 steps later
      for (i <- 0 until c.LATENCY)
      { 
        c.io.done.expect(false.B, s"done is set before latency is over - only waited $i cc")
        c.clock.step()
        c.io.last_state.poke(false)
      }

      // check done is high
      c.clock.step()
      c.io.done.expect(true.B, s"done is not set when expected at the end")
      c.clock.step()
      // TODO: maybe check if it goes back to low... or stays hight.. What does it need to be? 😵
      c.clock.step(10)
    }
  }
}

class LTCUnit_Setup_test extends AnyFlatSpec with ChiselScalatestTester {
  
  behavior of "LTCUnit"
  it should "write some weights to memory and run inference" in {
    val N_neurons = 12
    test(new LTCUnit(new LTCCoprocConfig())).withAnnotations(Seq(
      WriteVcdAnnotation,
      PrintFullStackTraceAnnotation)) { c =>
      c.clock.step() // step reset
      c.io.en.poke(false.B) // disable LTC Unit
      c.io.j.poke(0.U)
      c.io.k.poke(0.U)
      c.clock.step()

      // write N out neurons
      c.io.memWrite.N_out_neurons_write.bits.poke(N_neurons)
      c.io.memWrite.N_out_neurons_write.valid.poke(true)
      c.clock.step()
      c.io.memWrite.N_out_neurons_write.valid.poke(false)
      c.clock.step()

      // write sparcity
      println("writing someting to sparcity")
      var current_synapse_active = true
      c.io.memWrite.sparcity_write.valid.poke(true.B)
      for (i <- 0 until N_neurons)
      {
        for (j <- 0 until N_neurons)
        {
          val addr = i*N_neurons + j
          // println(s"writing $current_synapse_active to $addr")
          c.io.memWrite.sparcity_write.bits.writeAddr.poke(addr)
          c.io.memWrite.sparcity_write.bits.writeData.poke(current_synapse_active)
          c.clock.step()
          current_synapse_active = !current_synapse_active // just use every 2nd active to test writing
        }
      }
      c.io.memWrite.sparcity_write.valid.poke(false)

      c.clock.step()

      c.io.memWrite.weight_write.valid.poke(true)
      LTCUnit_WeightSel.all.foreach{ m =>
        println(s"writing something to $m")
        c.io.memWrite.weight_write.bits.writeSelect.poke(m)
        for (i <- 0 until (N_neurons*N_neurons / 2)) // only half of the weights since half of the synapses are inactive
        {
          c.io.memWrite.weight_write.bits.writeAddr.poke(i)
          c.io.memWrite.weight_write.bits.writeData.poke(i*2)
          c.clock.step()
        }
      }
      c.io.memWrite.weight_write.valid.poke(false)

      // activate unit and feed some states
      c.io.en.poke(true)
      c.io.unit_out.ready.poke(true) // we do not really care about data out here anyway
      c.io.fire.poke(true)
      var k = 0
      for (j <- 0 until N_neurons)
      {
        c.io.j.poke(j)
        for (i <- 0 until N_neurons)
        {
          c.io.k.poke(k)
          c.io.x_z1.poke(k)
          c.io.last_state.poke(i === (N_neurons-1))

          c.clock.step()

          c.io.fire.poke(false)
          c.io.last_state.poke(false)
          c.io.busy.expect(true)
          k += 1
        }
      }
      
      for (_ <- 0 until (c.LATENCY - 1))
      {
        c.io.done.expect(false)
        c.clock.step()
        c.io.busy.expect(true)
      }
      
      c.io.done.expect(true)
      c.clock.step()
      c.io.busy.expect(false)
      c.clock.step(200)
      c.io.busy.expect(false)
    }
  }
}


class LTCUnit_Inference_test extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "LTCUnit"

  
  // emit Verilog for test synthesis (this is independent of the tests below)
  val verilog_config = new LTCCoprocConfig(w=32, f=16)
  (new chisel3.stage.ChiselStage).emitVerilog(new LTCUnit(verilog_config))

  var dummy_counter_state = false

  // (16,true),(32,true),(16,false),(32,false)
  val a = Array(16, 32)
  val b = Array(true, false)
  for ((x,y) <- a.flatMap(x => b.map(y => (x,y))))
  {
    val W = x
    val useFC = y
    val F = W / 2
    val config = new LTCCoprocConfig(w=W, f=F)

    var header_file = "testdata/sandbox_ltc.h"
    var json_file = s"testdata/sandbox_fix${F}_rocc.json"
    var fc_str = ""
    if (useFC) {
      header_file = "testdata/sandbox_ltc_fc.h"
      json_file = s"testdata/sandbox_fc_fix${F}_rocc.json"
      fc_str = "fc "
    }

    // load model definition and weights
    val (units, ode_synapses, weigth_map, sparcity_matrix) = LTCTestDataUtil.ReadLTCModelFromHeader(header_file, F)

    // load rocc input and output values from json
    val ltc_rocc_values = LTCTestDataUtil.LoadLTCRoCCStimuli(json_file)
    
    it should s"run sandbox ${fc_str}model with fix$F" in {
      test(new LTCUnit(config)).withAnnotations(Seq(
        WriteVcdAnnotation,
        PrintFullStackTraceAnnotation)) { c =>
        c.clock.step() // step reset
        c.io.en.poke(false.B) // disable LTC Unit
        c.io.j.poke(0.U)
        c.io.k.poke(0.U)
        c.clock.step()

        val written_synapses = LTCTestUtil.WriteModelData2Unit(c.io.memWrite, c.clock, config,
          units, units, weigth_map, sparcity_matrix)
        assert(written_synapses == ode_synapses, "Not all synapses written")

        for (test_run <- 0 until ltc_rocc_values.size)
        {
          println(s"fix_$F test - iteration $test_run")
          val states = ltc_rocc_values(test_run)("states")
          val rev_activation = ltc_rocc_values(test_run)("rev_activation")
          val activation = ltc_rocc_values(test_run)("activation")

          // activate unit and feed some states
          c.io.en.poke(true)
          // c.clock.step(scala.util.Random.between(1,20))
          c.clock.step(5)
          fork{ timescope{
            c.io.fire.poke(true)
            c.clock.step()
          }}

          // feed input
          fork {
            var k = 0
            for (j <- 0 until units)
            {
              c.io.j.poke(j)
              for (i <- 0 until units)
              {
                c.io.k.poke(k)
                if (i > 0) {
                  if (dummy_counter_state) {
                    c.io.x_z1.poke(FixedPoint((i), W.W, F.BP))
                  } else {
                    c.io.x_z1.poke(FixedPoint(states(i-1), W.W, F.BP))
                  }
                }

                fork {timescope{
                  c.io.last_state.poke(i === (units-1))
                  c.clock.step()
                }}
      
                c.clock.step()
      
                c.io.busy.expect(true)
                k += 1
              }
              if (dummy_counter_state) {
                c.io.x_z1.poke(FixedPoint((units), W.W, F.BP))
              } else {
                c.io.x_z1.poke(FixedPoint(states(units-1), W.W, F.BP))
              }
            }
            c.clock.step()
          } 

          c.io.unit_out.ready.poke(true) // Note: this test is always ready to recieve data
          for (out_cnt <- 0 until units)
          {
            while (!c.io.unit_out.valid.peekBoolean())
            {
              c.clock.step()
            }
            c.io.unit_out.bits.act.expect(FixedPoint(activation(out_cnt), W.W, F.BP), s"act missmatch for neuron $out_cnt in test run $test_run")
            c.io.unit_out.bits.rev_act.expect(FixedPoint(rev_activation(out_cnt), W.W, F.BP), s"rev_act missmatch for neuron $out_cnt in test run $test_run")
            c.clock.step()
          }
          
          c.clock.step()
          c.io.done.expect(true)
          c.clock.step()
          c.io.busy.expect(false)
          // c.clock.step(scala.util.Random.between(10,200))
          c.clock.step(50)
          c.io.busy.expect(false)
        }
      }
    }
  }
}

class LTCCore_Inference_test extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "LTCCore"

  // emit Verilog for test synthesis (this is independent of the tests below)
  val verilog_config = new LTCCoprocConfig(w=32, f=16)
  (new chisel3.stage.ChiselStage).emitVerilog(new LTCCore(verilog_config))

  val config = new LTCCoprocConfig()

  println("preping test...")

  // TODO: extend for different number formats
  val F = config.f
  val W = config.w
  // load model definition and weights
  val (units, ode_synapses, weigth_map, sparcity_matrix) = LTCTestDataUtil.ReadLTCModelFromHeader("testdata/sandbox_ltc.h", F)

  // load rocc input and output values from json
  val ltc_rocc_values = LTCTestDataUtil.LoadLTCRoCCStimuli(s"testdata/sandbox_fix${F}_rocc.json")
  

  it should s"run ltc core first tries" in {
    test(new LTCCore(config)).withAnnotations(Seq(
    WriteVcdAnnotation,
    PrintFullStackTraceAnnotation)) { c =>
      println("starting test...")
      c.clock.step(10)

      // write Model config
      timescope {
        c.io.memWrite.Max_out_Neurons_write.bits.poke(scala.math.ceil(units.toDouble / config.N_Units).toInt) 
        c.io.memWrite.Max_out_Neurons_write.valid.poke(true)
        c.clock.step()
      }
      timescope {
        c.io.memWrite.Max_synapses_write.bits.poke((pow(units,2) / config.N_Units).toInt)
        c.io.memWrite.Max_synapses_write.valid.poke(true)
        c.clock.step()
      }
      timescope {
        c.io.memWrite.N_Neurons_write.bits.poke(units)
        c.io.memWrite.N_Neurons_write.valid.poke(true)
        c.clock.step()
      }


      var written_synapses = 0
      var written_neurons = 0
      val neurons_per_unit = blinag.DenseVector(Array.fill(config.N_Units)(units / config.N_Units))
      while (blinag.sum(neurons_per_unit) < units) {
        // find argmin of neurons_per_unit
        val i = blinag.argmin(neurons_per_unit)
        // add one
        neurons_per_unit(i) += 1
      }
      println("Using the following partitioning for the units")
      println(neurons_per_unit)
      for (i <- 0 until config.N_Units) {
        c.io.memWrite.UnitAddr.poke(i)
        written_synapses += LTCTestUtil.WriteModelData2Unit(c.io.memWrite.UnitMemWrite, c.clock, config, 
          units, neurons_per_unit(i), 
          weigth_map.view.mapValues(l => l.slice(written_synapses, l.length)).toMap,
          sparcity_matrix.map(_.slice(written_neurons, written_neurons+neurons_per_unit(i))))
        written_neurons += neurons_per_unit(i)
        c.clock.step()
        println(s"now written $written_synapses synapses")
      }
      assert(written_synapses == ode_synapses, "Not all synapses written")
      assert(written_neurons  == units, "Not all neurons written")

      println(s"written $written_synapses synapses")
      println(s"written $written_neurons neurons")

      // activate core and fire on 💥
      c.io.en.poke(true)
      fork{ timescope{
        c.io.fire.poke(true)
        c.clock.step()
      }}



      while (!c.io.done.peekBoolean()) {
        c.clock.step()
      }
      println("LTC Core done")

      c.clock.step(500)
    }
  }
}