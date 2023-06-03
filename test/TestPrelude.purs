module TestPrelude
  ( module FatPrelude
  , module Test.QuickCheck
  , module Test.Spec
  , module Test.Spec.QuickCheck
  , prop
  , pack
  , unpack
  ) where

import FatPrelude
import Data.List (toUnfoldable) as List

import Test.QuickCheck (class Testable, (===))
import Test.Spec.QuickCheck (quickCheck, quickCheck', quickCheckPure)
import Test.Spec (Spec, SpecT(..), after, afterAll, afterAll_, after_, around, aroundWith, around_, before, beforeAll, beforeAll_, beforeWith, before_, collect, describe, describeOnly, evaluateExample, focus, hoistSpec, it, itOnly, mapSpecTree, parallel, pending, pending', sequential)

prop :: forall prop. Testable prop => String -> prop -> Spec Unit
prop str = it str <<< quickCheck

pack :: List Char -> String
pack = fromCharArray <<< List.toUnfoldable

unpack :: String -> List Char
unpack = toUnfoldable <<< toCharArray
