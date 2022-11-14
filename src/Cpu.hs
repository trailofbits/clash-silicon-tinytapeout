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
        else (replace ptr (r - 1) rf, ptr, low)
    -- if R == RF[p], then P == p and cjump, else decrement R
    -- for P = p acts as unconditonal jump
    $(bitPattern "110_ppp") ->
      if r == (rf !! ppp)
        then (rf, ppp, high)
        else (replace ptr (r - 1) rf, ptr, low)
    -- add n to R
    -- for n = 0 acts as NOP
    $(bitPattern "1110_nn") -> (replace ptr (r + zeroExtend nn) rf, ptr, low)
    -- decrement R
    $(bitPattern "111100") -> (replace ptr (r - 1) rf, ptr, low)
    -- bitwise complement R
    $(bitPattern "111101") -> (replace ptr (complement r) rf, ptr, low)
    -- left shift R
    $(bitPattern "111110") -> (replace ptr (r `shiftL` 1) rf, ptr, low)
    -- right shift R
    $(bitPattern "111111") -> (replace ptr (r `shiftR` 1) rf, ptr, low)
    -- NOTE(jl): default case is not because this function isn't total,
    -- but instead defines what to do in the case of an undefined instruction.
    _ -> (rf, ptr, j)
  where
    r :: Register
    r = rf !! ptr
