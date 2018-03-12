{-# LANGUAGE ScopedTypeVariables #-}

module Hedgehog.RematchSpec where

import           Control.Rematch
import           Hedgehog
import qualified Hedgehog.Gen     as Gen
import qualified Hedgehog.Range   as Range
import           Hedgehog.Rematch (verify)

hprop_reverse :: Property
hprop_reverse = property $ do
  xs <- forAll $ Gen.list (Range.linear 0 42) Gen.alpha
  let actual = reverse $ reverse xs
  verify (is xs) actual

hprop_hasItem :: Property
hprop_hasItem = property $ do
  xs <- forAll $ Gen.list (Range.linear 0 43) Gen.alpha
  let actual = 't' : xs
  verify (hasItem $ equalTo 't') actual

hprop_hasNoItem :: Property
hprop_hasNoItem = property $ do
  xs <- forAll $ Gen.list (Range.linear 0 44) Gen.alpha
  let actual = filter ('t' /=) xs
  verify (isNot $ hasItem $ is 't') actual

hprop_hasSize :: Property
hprop_hasSize = property $ do
  xs <- forAll $ Gen.list (Range.linear 10 20) Gen.alpha
  verify (allOf [greaterThanOrEqual 10, lessThan 21]) $ length xs
