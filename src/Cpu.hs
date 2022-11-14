{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Cpu where

import Clash.Prelude
import qualified Prelude

type Instruction = BitVector 6

type Register =
  BitVector 5

type Pointer =
  BitVector 3

type Regfile = Vec 8 Register

type CJump = Bit

type State = (Regfile, Pointer, CJump)

cpu :: State -> Instruction -> State
cpu s@(rf, ptr, j) =
  \case
    -- literal value
    $(bitPattern "0_nnnnn") -> (replace ptr nnnnn rf, ptr, j)
    -- literal pointer
    $(bitPattern "100ppp") -> (rf, ppp, j)
    -- if R == 0, then jump to P, else increment pointer
    $(bitPattern "101ppp") -> if r == 0 then (rf, ppp, j) else (rf, ptr + 1, j)
    -- add literal value R
    $(bitPattern "1100nn") -> (replace ptr (r + zeroExtend nn) rf, ptr, j)
    -- subtract literal value R
    $(bitPattern "1101nn") -> (replace ptr (r - zeroExtend nn) rf, ptr, j)
    -- bitwise complement R
    $(bitPattern "11100.") -> (replace ptr (complement r) rf, ptr, j)
    -- left shift R
    $(bitPattern "11101.") -> (replace ptr (r `shiftL` 1) rf, ptr, j)
    -- increment pointer
    $(bitPattern "11110.") -> (rf, ptr + 1, j)
    -- decrement pointer
    $(bitPattern "11111.") -> (rf, ptr - 1, j)
    -- NOTE(jl): this is a circuit; can't leave an undefined branch.
    _ -> (rf, ptr, j)
  where
    r :: Register
    r = rf !! ptr
