{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}

module Props where

-- propositions over the register machine.

import Test.QuickCheck
import Control.Monad (void)

import Cpu

prop_foo :: Bool -> Bool
prop_foo _ = True

return []

main :: IO ()
main = void $quickCheckAll
