module Top where

import Clash.Annotations.TH
import Clash.Prelude
import Cpu

mooreF = cpu

mooreO :: State -> (Register, CJump)
mooreO (rf, ptr, j) = (rf !! ptr, j)

mooreM :: (HiddenClockResetEnable dom) => Signal dom Instruction -> Signal dom (Register, CJump)
mooreM = moore mooreF mooreO (unpack 0, 0, low)

top ::
  ("clk" ::: Clock System) ->
  ("rst" ::: Reset System) ->
  ("instr" ::: Signal System Instruction) ->
  ( "io_out" ::: Signal System Register,
    "cjump" ::: Signal System CJump
  )
top clk rst instr = exposeClockResetEnable (unbundle $ mooreM instr) clk rst enableGen

makeTopEntityWithName 'top "top"
