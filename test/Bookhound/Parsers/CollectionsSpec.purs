module Bookhound.Parsers.CollectionsSpec where

import TestPrelude

import Bookhound.Parser (runParser)
import Bookhound.ParserCombinators (is)
import Bookhound.Parsers.Collections (listOf, mapOf)
import Bookhound.Parsers.Char (alpha)

import Data.CodePoint.Unicode (isAlpha, isAscii)
import Data.Map as Map

spec :: Spec Unit
spec = describe "Bookhound.Parsers.Collections" $ do

  describe "listOf"
    $ prop "parses a list provided the element parser"
    $
      \(x :: Array Char) ->
        runParser (listOf alpha)

          ( "["
              <> intercalate ", "
                (fromCharArray <<< pure <$> filter isAlpha' x)
              <> "]"
          )
          ===
            Right (filter isAlpha' x)

  describe "mapOf"
    $ prop "parses a map provided the key and value parsers"
    $
      \(x :: Array (Char /\ Char)) ->
        runParser (mapOf (is ":") alpha alpha)
          ( "{"
              <> intercalate ", " (showMapEntry <$> filter areAlpha' x)
              <> "}"
          )
          ===
            Right (Map.fromFoldable $ filter areAlpha' x)

showMapEntry :: (Char /\ Char) -> String
showMapEntry (x /\ y) = fromCharArray (pure x) <> ": "
  <> fromCharArray (pure y)

areAlpha' :: (Char /\ Char) -> Boolean
areAlpha' (x /\ y) = isAlpha' x && isAlpha' y

isAlpha' :: Char -> Boolean
isAlpha' x = and $ [ isAlpha, isAscii ] <*> [ codePointFromChar x ]
