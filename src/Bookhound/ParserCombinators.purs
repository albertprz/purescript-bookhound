module Bookhound.ParserCombinators
  ( class IsMatch
  , is
  , isNot
  , inverse
  , oneOf
  , noneOf
  , satisfies
  , times
  , maybeTimes
  , anyTimes
  , someTimes
  , multipleTimes
  , anyTimesChar
  , someTimesChar
  , multipleTimesChar
  , within
  , maybeWithin
  , withinBoth
  , maybeWithinBoth
  , anySepBy
  , someSepBy
  , multipleSepBy
  , sepByOp
  , alt
  , parseAppend
  , withErrorFlipped
  , (<|>)
  , (<?>)
  , (<#>)
  , (->>-)
  , (|?)
  , (|*)
  , (|+)
  , (|++)
  , (||*)
  , (||+)
  , (||++)
  ) where

import FatPrelude

import Bookhound.Parser (Parser, allOf, anyOf, char, check, except, isMatch, withError)
import Bookhound.Utils.UnsafeRead (unsafeFromJust)
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

else instance IsMatch String where
  is =
    (fromCharArray <$> _) <<< traverse (isMatch (==) char) <<< toCharArray
  isNot =
    (fromCharArray <$> _) <<< traverse (isMatch (/=) char) <<< toCharArray
  inverse =
    (fromCharArray <$> _) <<< except ((|*) char) <<< (toCharArray <$> _)

else instance (UnsafeRead a, Show a) => IsMatch a where
  is n = unsafeRead <$> (is <<< show) n
  isNot n = unsafeRead <$> (isNot <<< show) n
  inverse p = unsafeRead <$> inverse (show <$> p)

oneOf :: forall a. IsMatch a => Array a -> Parser a
oneOf xs = anyOf $ is <$> xs

noneOf :: forall a. IsMatch a => Array a -> Parser a
noneOf xs = allOf $ isNot <$> xs

satisfies :: forall a. (a -> Boolean) -> Parser a -> Parser a
satisfies cond p = check "satisfies" cond p

alt :: forall a. Parser a -> Parser a -> Parser a
alt p1 p2 = anyOf [ p1, p2 ]

-- Times combinators
times :: forall a. Parser a -> Int -> Parser (Array a)
times p n = sequence $ p <$ (1 .. n)

maybeTimes :: forall a. Parser a -> Parser (Maybe a)
maybeTimes = (head <$> _) <<< check "maybeTimes" (not <<< hasMultiple) <<< anyTimes

anyTimes :: forall a. Parser a -> Parser (Array a)
anyTimes = (List.toUnfoldable <$> _) <<< helper
  where
  helper parser = (parser >>= (\x -> (x : _) <$> helper parser))
    <|> pure mempty

someTimes :: forall a. Parser a -> Parser (Array a)
someTimes = check "someTimes" hasSome <<< anyTimes

multipleTimes :: forall a. Parser a -> Parser (Array a)
multipleTimes = check "multipleTimes" hasMultiple <<< anyTimes

anyTimesChar :: Parser Char -> Parser String
anyTimesChar = (map fromCharArray) <<< anyTimes

someTimesChar :: Parser Char -> Parser String
someTimesChar = (map fromCharArray) <<< someTimes

multipleTimesChar :: Parser Char -> Parser String
multipleTimesChar = (map fromCharArray) <<< multipleTimes

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
parseAppend p1 p2 = (<>) <$> (toString <$> p1) <*> (toString <$> p2)

withErrorFlipped :: forall t31. Parser t31 -> String -> Parser t31
withErrorFlipped = flip withError

-- Parser Binary Operators
infixl 3 alt as <|>

infixl 6 times as <#>

infixl 6 withErrorFlipped as <?>

infixl 6 parseAppend as ->>-

-- Parser Frequency Unary Operators
infix 0 maybeTimes as |?

infix 0 anyTimes as |*

infix 0 someTimes as |+

infix 0 multipleTimes as |++

infix 0 anyTimesChar as ||*

infix 0 someTimesChar as ||+

infix 0 multipleTimesChar as ||++
