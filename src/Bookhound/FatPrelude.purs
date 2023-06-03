module FatPrelude
  ( module Prelude
  , module MapUtils
  , module StringUtils
  , module FoldableUtils
  , module ApplicativeUtils
  , module ArrayUtils
  , module UnsafeReadUtils
  , module Foldable
  , module Traversable
  , module Bifunctor
  , module Map
  , module Set
  , module Maybe
  , module Either
  , module Array
  , module List
  , module Tuple
  , module Tuple.Nested
  , module Unit
  , module Function
  , module Char
  , module String
  , module CodeUnits
  , module CodePoints
  , module Newtype
  , module Unicode
  , module PSCISupport
  ) where

import Prelude

import Bookhound.Utils.Applicative (extract) as ApplicativeUtils
import Bookhound.Utils.Array (class Range, range, (..), maybeToArray) as ArrayUtils
import Bookhound.Utils.Foldable (findJust, hasMultiple, hasNone, hasSome, stringify) as FoldableUtils
import Bookhound.Utils.Map (showMap) as MapUtils
import Bookhound.Utils.String (class ToString, indent, lines, toString) as StringUtils
import Bookhound.Utils.UnsafeRead (class UnsafeRead, unsafeRead) as UnsafeReadUtils
import Data.Array hiding (range, all, any, elem, notElem, find, findMap, foldM, intercalate, length, scanl, scanr, fold, foldMap, foldl, foldr, null, (..), (:)) as Array
import Data.Bifunctor (class Bifunctor, bimap, lmap, rmap) as Bifunctor
import Data.Char (fromCharCode, toCharCode) as Char
import Data.CodePoint.Unicode (isLower, isUpper) as Unicode
import Data.Either (Either(..), blush, choose, either, fromLeft, fromLeft', fromRight, fromRight', hush, isLeft, isRight, note, note') as Either
import Data.Foldable (class Foldable, all, and, any, elem, find, findMap, fold, foldM, foldMap, foldMapDefaultL, foldMapDefaultR, foldl, foldlDefault, foldr, foldrDefault, for_, indexl, indexr, intercalate, length, lookup, maximum, maximumBy, minimum, minimumBy, notElem, null, or, product, sequence_, sum, surround, surroundMap, traverse_) as Foldable
import Data.Function (compose, const, flip, identity, on, (#), ($), (<<<), (>>>)) as Function
import Data.List (List(..), (:)) as List
import Data.Map (Map) as Map
import Data.Maybe (Maybe(..), fromJust, fromMaybe, fromMaybe', isJust, isNothing, maybe, maybe', optional) as Maybe
import Data.Newtype (class Newtype, unwrap) as Newtype
import Data.Set (Set) as Set
import Data.String.CodePoints (CodePoint, codePointFromChar) as CodePoints
import Data.String.CodeUnits (fromCharArray, toCharArray, charAt, toChar) as CodeUnits
import Data.String.Common hiding (null) as String
import Data.Traversable (class Traversable, Accum, mapAccumL, mapAccumR, scanl, scanr, sequence, sequenceDefault, sequence_, traverse, traverseDefault, traverse_) as Traversable
import Data.Tuple (Tuple(..), curry, fst, snd, swap, uncurry) as Tuple
import Data.Tuple.Nested (type (/\), Tuple1, Tuple2, Tuple3, Tuple4, Tuple5, curry1, curry10, curry2, curry3, curry4, curry5, get1, get10, get2, get3, get4, get5, over1, over10, over2, over3, over4, over5, tuple1, tuple10, tuple2, tuple3, tuple4, tuple5, uncurry1, uncurry10, uncurry2, uncurry3, uncurry4, uncurry5, (/\)) as Tuple.Nested
import Data.Unit (Unit, unit) as Unit
import PSCI.Support (class Eval) as PSCISupport
import Prim hiding (Row) as Prim
