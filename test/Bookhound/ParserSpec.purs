module Bookhound.ParserSpec where

import TestPrelude hiding (length, toUpper)

import Bookhound.Parser (ParseError(..), ParseResult(..), Parser, allOf, anyOf, char, check, errorParser, exactly, except, isMatch, parse, runParser, withErrorN, withTransform)
import Data.Array (length)
import Data.CodePoint.Unicode (toUpper)

spec :: Spec Unit
spec = describe "Bookhound.Parser" $ do

  describe "Functor laws" $ do

    prop "Identity" $
      \x -> parse (identity <$> char) x
        === parse char x

    prop "Composition" $
      \x -> parse ((all isLower <<< toUpper) <$> codePoint) x
        ===
          parse ((all isLower <$> _) <<< (toUpper <$> _) $ codePoint) x

  describe "Applicative laws" $ do

    prop "Identity" $
      \x -> parse (pure identity <*> char) x
        ===
          parse char x

    prop "Homomorphism" $
      \x (y :: Array Int) -> parse (pure sum <*> pure y) x
        ===
          parse (pure (sum y)) x

    prop "Interchange" $
      \x (y :: Array Int) -> parse (pure length <*> pure y) x
        ===
          parse (pure (_ $ y) <*> pure length) x

  describe "Monad laws" $ do

    prop "Left identity" $
      \x -> parse (pure 'a' >>= isMatch (==) char) x
        ===
          parse (isMatch (==) char 'a') x

    prop "Right identity" $
      \x -> parse (char >>= pure) x
        ===
          parse char x

    prop "Associativity" $
      \x ->
        parse
          (char >>= (\y -> isMatch (<) char y >>= isMatch (>) char))
          x
          ===
            parse ((char >>= isMatch (<) char) >>= isMatch (>) char) x

  describe "char"
    $ prop "parses a single char"
    $
      \x -> parse char x
        ===
          case unpack x of
            (ch : rest) -> Result (pack rest) ch
            Nil -> Error UnexpectedEof

  describe "isMatch" $ do

    prop "works for ==" $
      \x y -> parse (isMatch (==) char y) x
        ===
          case unpack x of
            (ch : rest)
              | ch == y -> Result (pack rest) ch
              | otherwise -> Error $ UnexpectedChar ch
            Nil -> Error UnexpectedEof

    prop "works for /=" $
      \x y -> parse (isMatch (/=) char y) x
        ===
          case unpack x of
            (ch : rest)
              | ch /= y -> Result (pack rest) ch
              | otherwise -> Error $ UnexpectedChar ch
            Nil -> Error UnexpectedEof

  describe "check"
    $ prop "performs a check on the parse result"
    $
      \x -> parse (check "lower" isLower codePoint) x
        ===
          case unpack x of
            (ch : rest)
              | isLower $ codePointFromChar ch -> Result (pack rest)
                  (codePointFromChar ch)
              | otherwise -> Error $ NoMatch "lower"
            Nil -> Error UnexpectedEof

  describe "except"
    $ describe "when the second parser fails"
    $ prop "the first parser then runs"
    $
      \x -> parse (except char (char *> char)) x
        ===
          case unpack x of
            (ch : Nil) -> Result "" ch
            (_ : _) -> Error $ NoMatch "except"
            Nil -> Error UnexpectedEof

  describe "anyOf"
    $ prop "returns first parser success or last parser error if all fail"
        \x ->
          parse
            ( anyOf
                [ errorParser $ ErrorAt "firstError"
                , "firstSuccess" <$ char
                , "secondSuccess" <$ char
                , errorParser $ ErrorAt "secondError"
                , errorParser $ ErrorAt "lastError"
                ]
            )
            x
            ===
              case unpack x of
                (_ : rest) -> Result (pack rest) "firstSuccess"
                Nil -> Error $ ErrorAt "lastError"

  describe "allOf" $ do

    describe "when any parser fails"
      $ prop "returns first parser error"
      $
        \x ->
          parse
            ( allOf
                [ "firstSuccess" <$ char
                , "secondSuccess" <$ char
                , errorParser $ ErrorAt "firstError"
                ]
            )
            x
            ===
              case unpack x of
                (_ : _) -> Error $ ErrorAt "firstError"
                Nil -> Error UnexpectedEof

  describe "when all parsers succeed"
    $ prop "returns last parser success"
    $
      \x ->
        parse
          ( allOf
              [ "firstSuccess" <$ char
              , "secondSuccess" <$ char
              , "thirdSuccess" <$ char
              ]
          )
          x
          ===
            case unpack x of
              (_ : rest) -> Result (pack rest) "thirdSuccess"
              Nil -> Error UnexpectedEof

  describe "exactly"
    $ prop "returns an error when some chars remain after parsing"
    $
      \x -> parse (exactly char) x
        ===
          case unpack x of
            (ch : Nil) -> Result "" ch
            (_ : rest) -> Error $ ExpectedEof (pack rest)
            Nil -> Error UnexpectedEof

  describe "runParser"
    $ prop "parses exactly and wraps the result into an either"
    $
      \x -> runParser char x
        ===
          toEither (parse (exactly char) x)

  describe "withError"
    $ prop "includes error labels in order when running the parser"
    $
      \x ->
        runParser
          ( (/\) <$> withErrorN 2 "firstErr" char
              <*> withErrorN 1 "secondErr" char
          )
          x
          ===
            case unpack x of
              (ch1 : ch2 : Nil) -> Right (ch1 /\ ch2)
              (_ : _ : rest) -> Left
                [ ErrorAt "firstErr"
                , ErrorAt "secondErr"
                , ExpectedEof (pack rest)
                ]
              _ -> Left
                [ ErrorAt "firstErr"
                , ErrorAt "secondErr"
                , UnexpectedEof
                ]

  describe "withTransform"
    $ prop "transforms current parser with provided fn"
    $
      \x -> parse (withTransform (\p -> char *> p <* char) char) x
        ===
          parse (char *> char <* char) x

codePoint :: Parser CodePoint
codePoint = codePointFromChar <$> char

toEither :: forall a. ParseResult a -> Either (Array ParseError) a
toEither = case _ of
  Result _ a -> Right a
  Error pe -> Left $ pure pe
