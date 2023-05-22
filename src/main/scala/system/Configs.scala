// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package freechips.rocketchip.system

import org.chipsalliance.cde.config.Config
import freechips.rocketchip.subsystem._
import freechips.rocketchip.subsystem
import freechips.rocketchip.tile.LTCCoprocConfig

class WithJtagDTMSystem extends freechips.rocketchip.subsystem.WithJtagDTM
class WithDebugSBASystem extends freechips.rocketchip.subsystem.WithDebugSBA
class WithDebugAPB extends freechips.rocketchip.subsystem.WithDebugAPB

class BaseConfig extends Config(
  new WithDefaultMemPort ++
  new WithDefaultMMIOPort ++
  new WithDefaultSlavePort ++
  new WithTimebase(BigInt(1000000)) ++ // 1 MHz
  new WithDTS("freechips,rocketchip-unknown", Nil) ++
  new WithNExtTopInterrupts(2) ++
  new BaseSubsystemConfig
)

class DefaultConfig extends Config(new WithNBigCores(1) ++ new WithCoherentBusTopology ++ new BaseConfig)

class DefaultBufferlessConfig extends Config(new WithBufferlessBroadcastHub ++ new DefaultConfig)
class DefaultSmallConfig extends Config(new WithNSmallCores(1) ++ new WithCoherentBusTopology ++ new BaseConfig)
class DefaultRV32Config extends Config(new WithRV32 ++ new DefaultConfig)
class DefaultFP16Config extends Config(new WithFP16 ++ new DefaultConfig)

class BitManipCryptoConfig extends Config(new WithBitManip ++ new WithCryptoNIST ++ new WithCryptoSM ++ new DefaultConfig)
class BitManipCrypto32Config extends Config(new WithBitManip ++ new WithCryptoNIST ++ new WithCryptoSM ++ new DefaultRV32Config)

class HypervisorConfig extends Config(new WithHypervisor ++ new DefaultConfig)

class DualBankConfig extends Config(new WithNBanks(2) ++ new DefaultConfig)
class DualCoreConfig extends Config(new WithNBigCores(2) ++ new WithCoherentBusTopology ++ new BaseConfig)
class DualChannelConfig extends Config(new WithNMemoryChannels(2) ++ new DefaultConfig)
class EightChannelConfig extends Config(new WithNMemoryChannels(8) ++ new DefaultConfig)

class DualChannelDualBankConfig extends Config(
  new WithNMemoryChannels(2) ++
  new WithNBanks(4) ++ new DefaultConfig
)

class RoccExampleConfig extends Config(new WithRoccExample ++ new DefaultConfig)

class HeterogeneousTileExampleConfig extends Config(
  new WithNBigCores(n = 1) ++
  new WithNMedCores(n = 1) ++
  new WithNSmallCores(n = 1) ++
  new WithCoherentBusTopology ++
  new BaseConfig
)

class Edge128BitConfig extends Config(
  new WithEdgeDataBits(128) ++ new DefaultConfig
)
class Edge32BitConfig extends Config(
  new WithEdgeDataBits(32) ++ new DefaultConfig
)

class SingleChannelBenchmarkConfig extends Config(new DefaultConfig)
class DualChannelBenchmarkConfig extends Config(new WithNMemoryChannels(2) ++ new SingleChannelBenchmarkConfig)
class QuadChannelBenchmarkConfig extends Config(new WithNMemoryChannels(4) ++ new SingleChannelBenchmarkConfig)
class OctoChannelBenchmarkConfig extends Config(new WithNMemoryChannels(8) ++ new SingleChannelBenchmarkConfig)

class TinyConfig extends Config(
  new WithNoMemPort ++
  new WithNMemoryChannels(0) ++
  new WithNBanks(0) ++
  new With1TinyCore ++
  new WithIncoherentBusTopology ++
  new BaseConfig
)

class MemPortOnlyConfig extends Config(
  new WithNoMMIOPort ++
  new WithNoSlavePort ++
  new DefaultConfig
)

class MMIOPortOnlyConfig extends Config(
  new WithNoSlavePort ++
  new WithNoMemPort ++
  new WithNMemoryChannels(0) ++
  new WithNBanks(0) ++
  new WithIncoherentTiles ++
  new WithScratchpadsOnly ++
  new WithIncoherentBusTopology ++
  new DefaultConfig
)

class BaseFPGAConfig extends Config(new BaseConfig ++ new WithCoherentBusTopology)
class DefaultFPGAConfig extends Config(new WithNSmallCores(1) ++ new BaseFPGAConfig)

class CloneTileConfig extends Config(new WithCloneRocketTiles(7) ++ new WithNBigCores(1) ++ new WithCoherentBusTopology ++ new BaseConfig)

// class LTCConfig64 extends Config(new WithNSmallCores(1) ++ new BaseFPGAConfig ++ new WithRoccExample)
// class LTCConfig extends Config(new WithNSmallCores(1) ++ new WithRV32 ++ new BaseFPGAConfig ++ new WithRoccExample)

class LTCConfigFPU(
  ltc_config : LTCCoprocConfig
) extends Config(new WithNSmallCores32FPU(1) ++ new BaseFPGAConfig ++ new WithLTCRocc(ltc_config))
class LTCConfig(
  ltc_config : LTCCoprocConfig
) extends Config(new WithNSmallCores(1) ++ new WithRV32 ++ new BaseFPGAConfig ++ new WithLTCRocc(ltc_config))


class LTCConfigRBB extends Config(new WithJtagDTMSystem ++ new LTCConfig(new LTCCoprocConfig()))

class LTCConfigFPURBB extends Config(new WithJtagDTMSystem ++ new LTCConfigFPU(new LTCCoprocConfig()))
// class LTCConfigFPURBB extends Config(new WithJtagDTMSystem ++ new LTCConfig)


// Configs for evaluation cases
class LTCConfig_W32F16_small  extends Config(new LTCConfigFPU(new LTCCoprocConfig(w=32,f=16,N_Units=1)))
class LTCConfig_W32F16_medium extends Config(new LTCConfigFPU(new LTCCoprocConfig(w=32,f=16,N_Units=2)))
class LTCConfig_W32F16_large  extends Config(new LTCConfigFPU(new LTCCoprocConfig(w=32,f=16,N_Units=4)))

class LTCConfig_W16F8_small  extends Config(new LTCConfigFPU(new LTCCoprocConfig(w=16,f=8,N_Units=1)))
class LTCConfig_W16F8_medium extends Config(new LTCConfigFPU(new LTCCoprocConfig(w=16,f=8,N_Units=2)))
class LTCConfig_W16F8_large  extends Config(new LTCConfigFPU(new LTCCoprocConfig(w=16,f=8,N_Units=4)))

class LTCConfig_LargeMem_W32F16_small  extends Config(new LTCConfigFPU(new LTCCoprocConfig(w=32,f=16,N_Units=1, ramBlockArrdWidth=14)))
class LTCConfig_LargeMem_W32F16_medium extends Config(new LTCConfigFPU(new LTCCoprocConfig(w=32,f=16,N_Units=2, ramBlockArrdWidth=14)))
class LTCConfig_LargeMem_W32F16_large  extends Config(new LTCConfigFPU(new LTCCoprocConfig(w=32,f=16,N_Units=4, ramBlockArrdWidth=14)))

class LTCConfig_LargeMem_W16F8_small  extends Config(new LTCConfigFPU(new LTCCoprocConfig(w=16,f=8,N_Units=1, ramBlockArrdWidth=14)))
class LTCConfig_LargeMem_W16F8_medium extends Config(new LTCConfigFPU(new LTCCoprocConfig(w=16,f=8,N_Units=2, ramBlockArrdWidth=14)))
class LTCConfig_LargeMem_W16F8_large  extends Config(new LTCConfigFPU(new LTCCoprocConfig(w=16,f=8,N_Units=4, ramBlockArrdWidth=14)))