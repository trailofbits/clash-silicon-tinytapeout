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
    -- add literal value R
    $(bitPattern "101nnn") -> (replace ptr (r + zeroExtend nnn) rf, ptr, j)
    -- subtract literal value R
    $(bitPattern "110nnn") -> (replace ptr (r - zeroExtend nnn) rf, ptr, j)
    -- if R == 0, then P == 0 and cjump, else R--
    $(bitPattern "11100.") -> if r == 0 then (rf, 0, high) else (replace ptr (r - 1) rf, ptr, j)
    -- bitwise complement R
    $(bitPattern "11101.") -> (replace ptr (complement r) rf, ptr, j)
    -- left shift R
    $(bitPattern "11101.") -> (replace ptr (r `shiftL` 1) rf, ptr, j)
    -- NOTE(jl): this is a circuit; can't leave an undefined branch.
    _ -> (rf, ptr, j)
  where
    r :: Register
    r = rf !! ptr
