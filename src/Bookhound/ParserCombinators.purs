module Bookhound.ParserCombinators
  ( class IsMatch
  , is
  , isNot
  , inverse
  , oneOf
  , noneOf
  , isMatch
  , satisfy
  , times
  , some
  , many
  , multiple
  , manyChar
  , someChar
  , multipleChar
  , within
  , applyCons
  , applyTuple
  , maybeWithin
  , withinBoth
  , maybeWithinBoth
  , anySepBy
  , someSepBy
  , multipleSepBy
  , sepByOp
  , sepByOps
  , parseAppend
  , withErrorFlipped
  , timesFlipped
  , (<?>)
  , (<#>)
  , (<&>)
  , (<:>)
  , (</\>)
  , (->>-)
  , (|?)
  , (|*)
  , (|+)
  , (|++)
  , (||*)
  , (||+)
  , (||++)
  ) where

import Bookhound.FatPrelude

import Bookhound.Parser (Parser, allOf, anyOf, char, except, both, withError)
import Bookhound.Utils.String (charTraverse)
import Bookhound.Utils.UnsafeRead (unsafeFromJust)
import Control.Apply (lift2)
import Control.MonadPlus (class MonadPlus)
import Data.List as List
import Data.Unfoldable as Unfoldable

class IsMatch a where
  is :: a -> Parser a
  isNot :: a -> Parser a
  inverse :: Parser a -> Parser a

instance IsMatch Char where
  is = isMatch (==) char
  isNot = isMatch (/=) char
  inverse = except char

else instance IsMatch (Array Char) where
  is = traverse is
  isNot = traverse isNot
  inverse = except ((|*) char)

else instance IsMatch String where
  is = charTraverse is
  isNot = charTraverse isNot
  inverse = except ((||*) char)

else instance (UnsafeRead a, Show a) => IsMatch a where
  is = map unsafeRead <<< is <<< show
  isNot = map unsafeRead <<< isNot <<< show
  inverse = map unsafeRead <<< inverse <<< map show

oneOf :: forall a. IsMatch a => Array a -> Parser a
oneOf = anyOf <<< map is

noneOf :: forall a. IsMatch a => Array a -> Parser a
noneOf = allOf <<< map isNot

isMatch :: forall m a. MonadPlus m => (a -> a -> Boolean) -> m a -> a -> m a
isMatch cond ma c1 = satisfy (cond c1) ma

satisfy :: forall m a. MonadPlus m => (a -> Boolean) -> m a -> m a
satisfy cond ma = do
  c2 <- ma
  guard $ cond c2
  pure c2

many :: forall m a. MonadPlus m => m a -> m (Array a)
many = map List.toUnfoldable <<< helper
  where
  helper p = (p >>= \x -> Cons x <$> helper p) <|> pure mempty

some :: forall m a. MonadPlus m => m a -> m (Array a)
some = satisfy hasSome <<< many

multiple :: forall m a. MonadPlus m => m a -> m (Array a)
multiple = satisfy hasMultiple <<< many

-- Times combinators
times :: forall a. Int -> Parser a -> Parser (Array a)
times n p
  | n < 1 = sequence $ p <$ []
  | otherwise = sequence $ p <$ (1 .. n)

manyChar :: Parser Char -> Parser String
manyChar = map fromCharArray <<< many

someChar :: Parser Char -> Parser String
someChar = map fromCharArray <<< some

multipleChar :: Parser Char -> Parser String
multipleChar = map fromCharArray <<< multiple

-- Within combinators
within :: forall a b. Parser a -> Parser b -> Parser b
within p = extract p p

maybeWithin :: forall a b. Parser a -> Parser b -> Parser b
maybeWithin p = within ((|?) p)

withinBoth :: forall a b c. Parser a -> Parser b -> Parser c -> Parser c
withinBoth = extract

maybeWithinBoth :: forall a b c. Parser a -> Parser b -> Parser c -> Parser c
maybeWithinBoth p1 p2 = extract ((|?) p1) ((|?) p2)

-- Separated by combinators
sepBy
  :: forall a b
   . (Parser b -> Parser (Maybe b))
  -> (Parser b -> Parser (Array b))
  -> Parser a
  -> Parser b
  -> Parser (Array b)
sepBy freq1 freq2 sep p =
  (<>) <$> (Unfoldable.fromMaybe <$> freq1 p) <*> freq2 (sep *> p)

anySepBy :: forall a b. Parser a -> Parser b -> Parser (Array b)
anySepBy = sepBy (|?) (|*)

someSepBy :: forall a b. Parser a -> Parser b -> Parser (Array b)
someSepBy = sepBy (map Just) (|*)

multipleSepBy :: forall a b. Parser a -> Parser b -> Parser (Array b)
multipleSepBy = sepBy (map Just) (|+)

sepByOps :: forall a b. Parser a -> Parser b -> Parser (Array a /\ Array b)
sepByOps sep p = do
  x <- p
  y <- (|+) ((/\) <$> sep <*> p)
  pure $ ((fst <$> y) /\ cons x (snd <$> y))

sepByOp :: forall a b. Parser a -> Parser b -> Parser (a /\ Array b)
sepByOp sep p = lmap (unsafeFromJust <<< head) <$> sepByOps sep p

parseAppend
  :: forall a b
   . ToString a
  => ToString b
  => Parser a
  -> Parser b
  -> Parser String
parseAppend p1 p2 = append <$> map toString p1 <*> map toString p2

applyTuple :: forall f a b. Apply f => f a -> f b -> f (a /\ b)
applyTuple = lift2 Tuple

applyCons :: forall f a. Apply f => f a -> f (Array a) -> f (Array a)
applyCons = lift2 cons

withErrorFlipped :: forall t31. Parser t31 -> String -> Parser t31
withErrorFlipped = flip withError

timesFlipped :: forall a. Parser a -> Int -> Parser (Array a)
timesFlipped = flip times

-- Parser Binary Operators
infixl 6 timesFlipped as <#>

infixl 6 withErrorFlipped as <?>

infixl 6 both as <&>

infixl 6 applyTuple as </\>

infixl 6 applyCons as <:>

infixl 6 parseAppend as ->>-

-- Parser Frequency Unary Operators
infix 0 optional as |?

infix 0 many as |*

infix 0 some as |+

infix 0 multiple as |++

infix 0 manyChar as ||*

infix 0 someChar as ||+

infix 0 multipleChar as ||++
