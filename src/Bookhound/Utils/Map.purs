module Bookhound.Utils.Map where

import Prelude
import Data.Tuple.Nested ((/\))

import Data.Array (zip)
import Data.List as List
import Data.Map (Map, keys, values)
import Data.Set as Set

showMap :: forall a. String -> (String -> String) -> (a -> String) -> Map String a -> Array String
showMap sep showKey showValue mapping = (\(k /\ v) -> showKey k <> sep <> showValue v) <$> tuples
  where
  tuples = zip (Set.toUnfoldable $ keys mapping)
    (List.toUnfoldable $ values mapping)
