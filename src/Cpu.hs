{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Cpu where

import Clash.Prelude
import Prelude ()

type Instruction = BitVector 6

type Register =
  BitVector 5

type Pointer =
  BitVector 3

type Regfile = Vec 8 Register

type CJump = Bit

type State = (Regfile, Pointer, CJump)

cpu :: State -> Instruction -> State
cpu s@(rf, ptr, _) =
  \case
    -- literal value
    $(bitPattern "0_nnnnn") -> (replace ptr nnnnn rf, ptr, low)
    -- literal pointer
    $(bitPattern "100_ppp") -> (rf, ppp, low)
    -- add literal value R
    $(bitPattern "101_nnn") -> (replace ptr (r + zeroExtend nnn) rf, ptr, low)
    -- subtract literal value R
    $(bitPattern "110_nnn") -> (replace ptr (r - zeroExtend nnn) rf, ptr, low)
    -- if R == 0, then P == 0 and cjump, else decrement R
    $(bitPattern "11100.") -> if r == 0 then (rf, 0, high) else (replace ptr (r - 1) rf, ptr, low)
    -- bitwise complement R
    $(bitPattern "11101.") -> (replace ptr (complement r) rf, ptr, low)
    -- left shift R
    $(bitPattern "11110.") -> (replace ptr (r `shiftL` 1) rf, ptr, low)
    -- right shift R
    $(bitPattern "11111.") -> (replace ptr (r `shiftR` 1) rf, ptr, low)
    _ -> undefined
  where
    r :: Register
    r = rf !! ptr
