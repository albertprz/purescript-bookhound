module Bookhound.Parsers.Number (int, double, posInt, negInt, unsignedInt, hexInt) where

import Bookhound.FatPrelude

import Bookhound.Parser (Parser, withErrorN)
import Bookhound.ParserCombinators (is, oneOf, (->>-), (|+), (|?), (||+))
import Bookhound.Parsers.Char (dash, digit, dot, hexDigit, plus)
import Control.Alt ((<|>))

hexInt :: Parser Int
hexInt = withErrorN (-1) "Hex Int" $ unsafeRead
  <$> (is "0x" ->>- (|+) hexDigit)

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

double :: Parser Number
double = withErrorN (-1) "Double" $ unsafeRead
  <$> int
  ->>- (decimals ->>- (|?) expn <|> expn)
  where
  decimals = dot ->>- unsignedInt
  expn = oneOf [ 'e', 'E' ] ->>- int
