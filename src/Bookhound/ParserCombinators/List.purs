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
import Data.List as List

-- Frequency combinators
many :: forall a. Parser a -> Parser (List a)
many p = (p >>= \x -> pure x <:> many p) <|> pure mempty

some :: forall a. Parser a -> Parser (List a)
some = satisfy hasSome <<< many

multiple :: forall a. Parser a -> Parser (List a)
multiple = satisfy hasMultiple <<< many

-- Separated by combinators
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

-- Apply Binary Operators
infixl 6 applyCons as <:>

-- Frequency Unary Operators
infix 0 optional as |?

infix 0 many as |*

infix 0 some as |+

infix 0 multiple as |++
