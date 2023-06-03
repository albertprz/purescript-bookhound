module Bookhound.ParserCombinatorsSpec where

import TestPrelude

import Bookhound.Parser (ParseError(..), ParseResult(..), Parser, anyOf, char, parse)
import Bookhound.ParserCombinators (anySepBy, anyTimes, is, maybeTimes, maybeWithin, maybeWithinBoth, multipleSepBy, multipleTimes, someSepBy, someTimes, times, within, withinBoth, (->>-), (|*), (|+), (|?))
import Data.String as String

spec :: Spec Unit
spec = describe "Bookhound.ParserCombinators" $ do

  describe "times"
    $ prop "applies a parser n times sequentially"
    $
      \x (n :: SmallInt) -> parse (times (unwrap n) char) x
        ===
          if String.length x >= unwrap n then
            Result (String.drop (unwrap n) x)
              (toCharArray $ String.take (unwrap n) x)
          else
            Error UnexpectedEof

  describe "maybeTimes"
    $ prop "applies a parser 1 or 0 times"
    $
      \x -> parse (maybeTimes char) x
        ===
          (head <$> parseTimes char [ 0, 1 ] x)

  describe "anyTimes"
    $ prop "applies a parser any number of times"
    $
      \x -> parse (anyTimes char) x
        ===
          parseTimes char (0 .. (String.length x + 10)) x

  describe "someTimes"
    $ prop "applies a parser at least once"
    $
      \x -> parse (someTimes char) x
        ===
          replaceError "someTimes"
            (parseTimes char (1 .. max 1 (String.length x + 10)) x)

  describe "multipleTimes"
    $ prop "applies a parser at least twice"
    $
      \x -> parse (multipleTimes char) x
        ===
          replaceError "multipleTimes"
            (parseTimes char (2 .. max 2 (String.length x + 10)) x)

  describe "withinBoth"
    $ prop "applies a parser surrounded by 2 parsers"
    $
      \x (y :: Char) (z :: Char) ->
        parse (withinBoth (is y) (is z) char) x
          ===
            parse (is y *> char <* is z) x

  describe "maybeWithinBoth"
    $ prop "applies a parser surrounded by 2 optional parsers"
    $
      \x (y :: Char) (z :: Char) ->
        parse (maybeWithinBoth (is y) (is z) char) x
          ===
            parse (((|?) $ is y) *> char <* ((|?) $ is z)) x

  describe "within"
    $ prop "applies a parser surrounded by a parser"
    $
      \x (y :: Char) ->
        parse (within (is y) char) x
          ===
            parse (is y *> char <* is y) x

  describe "maybeWithin"
    $ prop "applies a parser surrounded by a optional parsers"
    $
      \x (y :: Char) ->
        parse (maybeWithin (is y) char) x
          ===
            parse (((|?) $ is y) *> char <* ((|?) $ is y)) x

  describe "anySepBy"
    $ prop "applies a parser separated by a parser any number of times"
    $
      \x (y :: Char) ->
        parse (anySepBy (is y) char) x
          ===
            parse
              ( (<>) <$> (maybeToArray <$> ((|?) char))
                  <*> ((|*) (is y *> char))
              )
              x

  describe "someSepBy"
    $ prop "applies a parser separated by a parser at least once"
    $
      \x (y :: Char) ->
        parse (someSepBy (is y) char) x
          ===
            parse ((cons) <$> char <*> ((|*) (is y *> char))) x

  describe "multipleSepBy"
    $ prop "applies a parser separated by a parser at least twice"
    $
      \x (y :: Char) ->
        parse (multipleSepBy (is y) char) x
          ===
            parse ((cons) <$> char <*> ((|+) (is y *> char))) x

  describe "->>-"
    $ prop "concats results of 2 parsers that can be converted to Strings"
    $
      \x (y :: Char) (z :: Char) ->
        parse (is y ->>- is z) x
          ===
            parse (fromCharArray <$> is [ y, z ]) x

parseTimes :: forall a. Parser a -> Array Int -> String -> ParseResult (Array a)
parseTimes p ns = parse $ anyOf ((_ `times` p) <$> reverse ns)

replaceError :: forall a. String -> ParseResult a -> ParseResult a
replaceError err (Error (NoMatch _)) = Error $ NoMatch err
replaceError err (Error _) = Error $ NoMatch err
replaceError _ result = result

