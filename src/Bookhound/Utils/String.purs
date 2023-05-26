module Bookhound.Utils.String where

import Prelude

import Data.Array (fold, replicate)
import Data.Foldable (class Foldable, foldMap)
import Data.List (intercalate)
import Data.String (Pattern(..), split)
import Data.String.CodeUnits (singleton)

class ToString a where
  toString :: a -> String

instance ToString Char where
  toString = singleton

instance ToString String where
  toString = identity

instance ToString Int where
  toString = show

instance ToString Number where
  toString = show

instance (ToString a, Foldable m) => ToString (m a) where
  toString = foldMap toString

lines :: String -> Array String
lines = split (Pattern "\n")

indent :: Int -> String -> String
indent n str = intercalate "\n" $ indentLine <$> lines str
  where
  indentLine = ((fold $ replicate n " ") <> _)
