make -j8 CONFIG=freechips.rocketchip.system.LTCConfig_W32F16_small
make -j8 CONFIG=freechips.rocketchip.system.LTCConfig_W32F16_medium
make -j8 CONFIG=freechips.rocketchip.system.LTCConfig_W32F16_large
make -j8 CONFIG=freechips.rocketchip.system.LTCConfig_W16F8_small
make -j8 CONFIG=freechips.rocketchip.system.LTCConfig_W16F8_medium
make -j8 CONFIG=freechips.rocketchip.system.LTCConfig_W16F8_large


make debug -j8 CONFIG=freechips.rocketchip.system.LTCConfig_W32F16_small
make debug -j8 CONFIG=freechips.rocketchip.system.LTCConfig_W32F16_medium
make debug -j8 CONFIG=freechips.rocketchip.system.LTCConfig_W32F16_large
make debug -j8 CONFIG=freechips.rocketchip.system.LTCConfig_W16F8_small
make debug -j8 CONFIG=freechips.rocketchip.system.LTCConfig_W16F8_medium
make debug -j8 CONFIG=freechips.rocketchip.system.LTCConfig_W16F8_large