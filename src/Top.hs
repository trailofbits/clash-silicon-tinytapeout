module Top where

import Clash.Annotations.TH
import Clash.Prelude

import Cpu

mooreF = cpu

mooreO :: State -> Port
mooreO (rf, ptr) = 0 :> 0 :> 0 :> unpack (rf !! ptr)

regMachineM :: (HiddenClockResetEnable dom) => Signal dom Instruction -> Signal dom Port
regMachineM = moore mooreF mooreO (unpack 0, 0)

top ::
  ("clk" ::: Clock System) ->
  ("rst" ::: Reset System) ->
  ("instr" ::: Signal System Instruction) ->
  ("io_out" ::: Signal System Port)
top clk rst instr = exposeClockResetEnable (regMachineM instr) clk rst enableGen

makeTopEntityWithName 'top "top"
