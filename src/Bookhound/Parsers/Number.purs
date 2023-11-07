module Bookhound.Parsers.Number (int, double, posInt, negInt, unsignedInt, hexInt, octInt, intLike) where

import Bookhound.FatPrelude

import Bookhound.Parser (Parser, withErrorN)
import Bookhound.ParserCombinators (is, oneOf, (->>-), (|+), (|?), (||+))
import Bookhound.Parsers.Char (dash, digit, dot, plus)
import Control.Alt ((<|>))
import Data.String as String

hexInt :: Parser Int
hexInt = withErrorN (-1) "Hex Int" $ unsafeRead
  <$>
    ( is "0x" ->>-
        (|+) (digit <|> oneOf ('A' .. 'F') <|> (oneOf ('a' .. 'f')))
    )

octInt :: Parser Int
octInt = withErrorN (-1) "Oct Int" $ unsafeRead
  <$> (is "0o" ->>- (|+) (oneOf ('0' .. '7')))

unsignedInt :: Parser Int
unsignedInt = withErrorN (-1) "Unsigned Int" $ unsafeRead
  <$> (||+) digit

posInt :: Parser Int
posInt = withErrorN (-1) "Positive Int" $ unsafeRead
  <$> ((|?) plus *> (||+) digit)

negInt :: Parser Int
negInt = withErrorN (-1) "Negative Int" $ unsafeRead
  <$> (dash ->>- (||+) digit)

int :: Parser Int
int = withErrorN (-1) "Int" $ negInt <|> posInt

intLike :: Parser Int
intLike = withErrorN (-1) "Int Like" $ parser <|> int
  where
  parser = do
    n1 <- show <$> int
    n2 <- show <$> (dot *> unsignedInt)
    expNum <- oneOf [ 'e', 'E' ] *> int
    guard (String.length n1 + String.length n2 <= expNum)
    pure <<< unsafeRead $ n1 <> "." <> n2 <> "E" <> show expNum

double :: Parser Number
double = withErrorN (-1) "Double" $ unsafeRead
  <$> (|?) dash
  ->>- posInt
  ->>- decimals
  ->>- (|?) expn
  where
  decimals = dot ->>- unsignedInt
  expn = oneOf [ 'e', 'E' ] ->>- int
