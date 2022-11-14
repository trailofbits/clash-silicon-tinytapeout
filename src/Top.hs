module Top where

import Clash.Annotations.TH
import Clash.Prelude
import Cpu

type Port = Vec 8 Bit

--
-- construct a moore machine from the function over the cpu;
-- 1. a function `mooreF` that maps a state and input to next state
--      State -> Instruction -> State
-- 2. a function `mooreO` that maps a state to an output
--      State -> Port
-- 3. an initial state
--
mooreF = cpu

mooreO :: State -> Port
mooreO (rf, ptr, j) = j :> 0 :> 0 :> unpack (rf !! ptr)

mooreM :: (HiddenClockResetEnable dom) => Signal dom Instruction -> Signal dom Port
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
{-# ANN
  topEntity
  ( Synthesize
      { t_name = "top",
        t_inputs =
          [ PortName "clk",
            PortName "rst",
            PortName "instr"
          ],
        t_output =
          PortName "io_out"
      }
  )
  #-}
topEntity ::
  Clock System ->
  Reset System ->
  Signal System Instruction ->
  Signal System Port
topEntity clk rst instr = exposeClockResetEnable (mooreM instr) clk rst enableGen
