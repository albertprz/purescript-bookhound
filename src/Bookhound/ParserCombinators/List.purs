module Bookhound.ParserCombinators.List
  ( many
  , some
  , multiple
  , manySepBy
  , someSepBy
  , multipleSepBy
  , sepByOp
  , sepByOps
  , (|?)
  , (|*)
  , (|+)
  , (|++)
  ) where

import Bookhound.FatPrelude

import Bookhound.Parser (Parser, satisfy)
import Bookhound.Utils.UnsafeRead (unsafeFromJust)
import Control.Apply (lift2)
import Control.MonadPlus (class MonadPlus)
import Data.List as List

many :: forall m a. MonadPlus m => m a -> m (List a)
many p = (p >>= \x -> pure x <:> many p) <|> pure mempty

some :: forall m a. MonadPlus m => m a -> m (List a)
some = satisfy hasSome <<< many

multiple :: forall m a. MonadPlus m => m a -> m (List a)
multiple = satisfy hasMultiple <<< many

sepBy
  :: forall a b
   . (Parser b -> Parser (Maybe b))
  -> (Parser b -> Parser (List b))
  -> Parser a
  -> Parser b
  -> Parser (List b)
sepBy freq1 freq2 sep p =
  maybeCons <$> freq1 p <*> freq2 (sep *> p)
  where
  maybeCons Nothing = identity
  maybeCons (Just x) = Cons x

manySepBy :: forall a b. Parser a -> Parser b -> Parser (List b)
manySepBy = sepBy optional many

someSepBy :: forall a b. Parser a -> Parser b -> Parser (List b)
someSepBy = sepBy (map Just) many

multipleSepBy :: forall a b. Parser a -> Parser b -> Parser (List b)
multipleSepBy = sepBy (map Just) some

sepByOps :: forall a b. Parser a -> Parser b -> Parser (List a /\ List b)
sepByOps sep p = do
  x <- p
  y <- (|+) (Tuple <$> sep <*> p)
  pure $ map fst y /\ x : map snd y

sepByOp :: forall a b. Parser a -> Parser b -> Parser (a /\ List b)
sepByOp sep p = lmap (unsafeFromJust <<< List.head) <$> sepByOps sep p

applyCons :: forall f a. Apply f => f a -> f (List a) -> f (List a)
applyCons = lift2 Cons

infix 0 optional as |?

infix 0 many as |*

infix 0 some as |+

infix 0 multiple as |++

infixl 6 applyCons as <:>
