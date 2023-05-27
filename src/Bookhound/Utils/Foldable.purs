module Bookhound.Utils.Foldable where

import Prelude

import Bookhound.Utils.String (indent)
import Data.Array (fromFoldable) as Array
import Data.Array.Partial (tail) as Partial
import Data.Foldable (class Foldable, intercalate, null)
import Data.Foldable as Foldable
import Data.Maybe (Maybe, isJust)
import Partial.Unsafe (unsafePartial)

hasNone :: forall a t. Foldable t => t a -> Boolean
hasNone = null

hasSome :: forall a t. Foldable t => t a -> Boolean
hasSome = not <<< hasNone

hasMultiple :: forall a t. Foldable t => t a -> Boolean
hasMultiple xs =
  hasSome arr && hasSome (unsafePartial $ Partial.tail arr)
  where
  arr = Array.fromFoldable xs

findJust :: forall a t. Foldable t => t (Maybe a) -> Maybe a
findJust ms = join $ Foldable.find isJust ms

stringify
  :: forall t
   . Foldable t
  => String
  -> String
  -> String
  -> Int
  -> t String
  -> String
stringify sep start end n xs = start <> indent n str <> end
  where
  str = intercalate sep xs
