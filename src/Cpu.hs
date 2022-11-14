{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Cpu where

import Clash.Prelude
import Prelude ()

type Instruction = BitVector 6

type Register = BitVector 5

type Pointer = BitVector 3

type Regfile = Vec 8 Register

type CJump = Bit

type State = (Regfile, Pointer, CJump)

cpu :: State -> Instruction -> State
cpu (rf, ptr, j) =
  \case
    -- literal value n
    $(bitPattern "0_nnnnn") -> (replace ptr nnnnn rf, ptr, low)
    -- literal pointer p
    $(bitPattern "100_ppp") -> (rf, ppp, low)
    -- if R == 0,     then P == p and cjump, else decrement R
    $(bitPattern "101_ppp") ->
      if r == 0
        then (rf, ppp, high)
        else (rf, ptr, low)
    -- add n to R
    -- for n = 0 acts as NOP
    $(bitPattern "1100nn") -> (replace ptr (r + zeroExtend nn) rf, ptr, low)
    -- subtract n from R
    $(bitPattern "1101nn") -> (replace ptr (r - zeroExtend nn) rf, ptr, low)
    -- bitwise complement R
    $(bitPattern "11101.") -> (replace ptr (complement r) rf, ptr, low)
    -- left shift R
    $(bitPattern "11110.") -> (replace ptr (r `shiftL` 1) rf, ptr, low)
    -- right shift R
    $(bitPattern "11111.") -> (replace ptr (r `shiftR` 1) rf, ptr, low)
    -- NOTE(jl): default case is not because this function isn't total,
    -- but instead defines what to do in the case of an undefined instruction.
    _ -> (rf, ptr, j)
  where
    r :: Register
    r = rf !! ptr
