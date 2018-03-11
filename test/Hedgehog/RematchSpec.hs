{-# LANGUAGE ScopedTypeVariables #-}

module Hedgehog.RematchSpec where

import           Control.Rematch
import           Hedgehog
import qualified Hedgehog.Gen     as Gen
import qualified Hedgehog.Range   as Range
import           Hedgehog.Rematch (verify)

hprop_reverse :: Property
hprop_reverse = property $ do
  xs <- forAll $ Gen.list (Range.linear 0 100) Gen.alpha
  verify (is xs) $ reverse (reverse ('t' : xs))
