{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Props where

-- propositions over the register machine.

import Control.Monad (void)
import Cpu
import Test.QuickCheck

prop_foo :: Bool -> Bool
prop_foo _ = True

return []

main :: IO ()
main = void $quickCheckAll
