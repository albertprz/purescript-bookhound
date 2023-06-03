module TestPrelude
  ( module FatPrelude
  , module Test.QuickCheck
  , module Test.Spec
  , module Test.Spec.QuickCheck
  , module Control.Monad.Gen
  , module Data.Newtype
  , prop
  , pack
  , unpack
  , PositiveInt(..)
  , NegativeInt(..)
  , SmallInt(..)

  ) where

import FatPrelude

import Control.Monad.Gen (class MonadGen, Size, chooseBool, chooseFloat, chooseInt, elements, filtered, frequency, oneOf, resize, sized, suchThat, unfoldable)
import Data.List (toUnfoldable) as List
import Data.Newtype (class Newtype, ala, alaF, collect, modify, over, overF, overF2, un, under, under2, underF, underF2, unwrap, wrap)
import Test.QuickCheck (class Arbitrary, class Testable, (===))
import Test.Spec (Spec, SpecT(..), after, afterAll, afterAll_, after_, around, aroundWith, around_, before, beforeAll, beforeAll_, beforeWith, before_, describe, describeOnly, evaluateExample, focus, hoistSpec, it, itOnly, mapSpecTree, parallel, pending, pending', sequential)
import Test.Spec.QuickCheck (quickCheck, quickCheck', quickCheckPure)

prop :: forall prop. Testable prop => String -> prop -> Spec Unit
prop str = it str <<< quickCheck

pack :: List Char -> String
pack = fromCharArray <<< List.toUnfoldable

unpack :: String -> List Char
unpack = toUnfoldable <<< toCharArray

newtype PositiveInt = PositiveInt Int

newtype NegativeInt = NegativeInt Int

newtype SmallInt = SmallInt Int

derive instance Newtype PositiveInt _

derive instance Newtype NegativeInt _

derive instance Newtype SmallInt _

instance Arbitrary PositiveInt where
  arbitrary = wrap <$> chooseInt 1 1000000

instance Arbitrary NegativeInt where
  arbitrary = wrap <$> chooseInt (-1000000) (-1)

instance Arbitrary SmallInt where
  arbitrary = wrap <$> chooseInt (-1000) 1000
