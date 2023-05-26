module Bookhound.Utils.UnsafeRead where

import Prelude

import Data.Int as Int
import Data.Number as Number
import Data.Maybe (Maybe, fromJust)
import Partial.Unsafe (unsafePartial)

class UnsafeRead a where
  unsafeRead :: String -> a

instance UnsafeRead Int where
  unsafeRead = unsafeFromJust <<< Int.fromString

instance UnsafeRead Number where
  unsafeRead = unsafeFromJust <<< Number.fromString

unsafeFromJust :: forall a. Maybe a -> a
unsafeFromJust = unsafePartial fromJust
