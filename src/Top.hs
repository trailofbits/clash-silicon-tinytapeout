module Top where

import Clash.Annotations.TH
import Clash.Prelude
import Cpu

--
-- construct a moore machine from the function over the cpu;
-- 1. a function `mooreF` that maps a state and input to next state
--      State -> Instruction -> State
-- 2. a function `mooreO` that maps a state to an output
--      State -> (Register, CJump)
-- 3. an initial state
--
mooreF = cpu

mooreO :: State -> (Register, CJump)
mooreO (rf, ptr, j) = (rf !! ptr, j)

mooreM :: (HiddenClockResetEnable dom) => Signal dom Instruction -> Signal dom (Register, CJump)
mooreM = moore mooreF mooreO (rf0, p0, j0)
  where
    -- initial cpu states
    rf0 :: Regfile
    rf0 = unpack 0
    p0 :: Pointer
    p0 = 0
    j0 :: CJump
    j0 = low

--
-- circuit top-level. expose moore machine with clock and reset.
--
top ::
  ("clk" ::: Clock System) ->
  ("rst" ::: Reset System) ->
  ("instr" ::: Signal System Instruction) ->
  ( "io_out" ::: Signal System Register,
    "cjump" ::: Signal System CJump
  )
top clk rst instr = exposeClockResetEnable (unbundle $ mooreM instr) clk rst enableGen

makeTopEntityWithName 'top "top"
