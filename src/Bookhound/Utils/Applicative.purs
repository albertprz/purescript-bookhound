module Bookhound.Utils.Applicative where

import Control.Alternative

extract :: forall a1 a2 b m. Applicative m => m a1 -> m a2 -> m b -> m b
extract start end inner = start *> inner <* end
