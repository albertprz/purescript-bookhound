module Bookhound.ParserSpec where

import TestPrelude hiding (length, toUpper)

import Bookhound.Parser (ParseError(..), ParseResult(..), Parser, allOf, anyOf, anyChar, exactly, except, parse, runParser, withErrorN, satisfy, withTransform)
import Control.Monad.Error.Class (throwError)
import Control.MonadPlus (class MonadPlus)
import Data.Array (length)
import Data.CodePoint.Unicode (toUpper)

spec :: Spec Unit
spec = describe "Bookhound.Parser" do

  describe "Functor laws" do

    prop "Identity"
      $ \x -> parse (identity <$> anyChar) x
          === parse anyChar x

    prop "Composition"
      $ \x -> parse ((all isLower <<< toUpper) <$> codePoint) x
          === parse ((all isLower <$> _) <<< (toUpper <$> _) $ codePoint) x

  describe "Applicative laws" do

    prop "Identity"
      $ \x -> parse (pure identity <*> anyChar) x
          === parse anyChar x

    prop "Homomorphism"
      $ \x (y :: Array Int) -> parse (pure sum <*> pure y) x
          === parse (pure (sum y)) x

    prop "Interchange"
      $ \x (y :: Array Int) -> parse (pure length <*> pure y) x
          === parse (pure (_ $ y) <*> pure length) x

  describe "Monad laws" do

    prop "Left identity"
      $ \x -> parse (pure 'a' >>= isMatch (==) anyChar) x
          === parse (isMatch (==) anyChar 'a') x

    prop "Right identity"
      $ \x -> parse (anyChar >>= pure) x
          === parse anyChar x

    prop "Associativity"
      $ \x ->
          parse
            (anyChar >>= (\y -> isMatch (<) anyChar y >>= isMatch (>) anyChar))
            x === parse ((anyChar >>= isMatch (<) anyChar) >>= isMatch (>) anyChar) x

  describe "anyChar"
    $ prop "parses a single anyChar"
    $ \x -> parse anyChar x
        === case unpack x of
          (ch : rest) -> Result (pack rest) ch
          Nil -> Error UnexpectedEof

  describe "isMatch" $ do

    prop "works for =="
      $ \x y -> parse (isMatch (==) anyChar y) x
          === case unpack x of
            (ch : rest)
              | ch == y -> Result (pack rest) ch
              | hasSome rest -> Error $ ExpectedEof $ pack rest
            _ -> Error UnexpectedEof

    prop "works for /="
      $ \x y -> parse (isMatch (/=) anyChar y) x
          === case unpack x of
            (ch : rest)
              | ch /= y -> Result (pack rest) ch
            _ -> Error UnexpectedEof

  describe "satisfy"
    $ prop "performs a check on the parse result"
    $ \x -> parse (satisfy isLower codePoint) x
        === case unpack x of
          (ch : rest)
            | isLower $ codePointFromChar ch -> Result (pack rest)
                (codePointFromChar ch)
            | hasSome rest -> Error $ ExpectedEof $ pack rest
          _ -> Error UnexpectedEof

  describe "except"
    $ describe "when the second parser fails"
    $ prop "the first parser then runs"
    $ \x -> parse (except anyChar (anyChar *> anyChar)) x
        === case unpack x of
          (ch : Nil) -> Result "" ch
          (_ : _) -> Error $ ExpectedEof x
          Nil -> Error UnexpectedEof

  describe "anyOf"
    $ prop "returns first parser success or last parser error if all fail"
    $ \x ->
        parse
          ( anyOf
              [ throwError $ ErrorAt "firstError"
              , "firstSuccess" <$ anyChar
              , "secondSuccess" <$ anyChar
              , throwError $ ErrorAt "secondError"
              , throwError $ ErrorAt "lastError"
              ]
          )
          x === case unpack x of
          (_ : rest) -> Result (pack rest) "firstSuccess"
          Nil -> Error $ ErrorAt "lastError"

  describe "allOf" $ do

    describe "when any parser fails"
      $ prop "returns first parser error"
      $ \x ->
          parse
            ( allOf
                [ "firstSuccess" <$ anyChar
                , "secondSuccess" <$ anyChar
                , throwError $ ErrorAt "firstError"
                ]
            )
            x === case unpack x of
            (_ : _) -> Error $ ErrorAt "firstError"
            Nil -> Error UnexpectedEof

    describe "when all parsers succeed"
      $ prop "returns last parser success"
      $ \x ->
          parse
            ( allOf
                [ "firstSuccess" <$ anyChar
                , "secondSuccess" <$ anyChar
                , "thirdSuccess" <$ anyChar
                ]
            )
            x === case unpack x of
            (_ : rest) -> Result (pack rest) "thirdSuccess"
            Nil -> Error UnexpectedEof

  describe "exactly"
    $ prop "returns an error when some chars remain after parsing"
    $ \x -> parse (exactly anyChar) x
        === case unpack x of
          (ch : Nil) -> Result "" ch
          (_ : rest) -> Error $ ExpectedEof (pack rest)
          Nil -> Error UnexpectedEof

  describe "runParser"
    $ prop "parses exactly and wraps the result into an either"
    $ \x -> runParser anyChar x
        === toEither (parse (exactly anyChar) x)

  describe "withError"
    $ prop "includes error labels in order when running the parser"
    $ \x ->
        runParser
          ( (/\) <$> withErrorN 2 "firstErr" anyChar
              <*> withErrorN 1 "secondErr" anyChar
          )
          x === case unpack x of
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
    $ \x -> parse (withTransform (\p -> anyChar *> p <* anyChar) anyChar) x
        === parse (anyChar *> anyChar <* anyChar) x

isMatch :: forall m a. MonadPlus m => (a -> a -> Boolean) -> m a -> a -> m a
isMatch cond ma c1 = satisfy (cond c1) ma

codePoint :: Parser CodePoint
codePoint = codePointFromChar <$> anyChar

toEither :: forall a. ParseResult a -> Either (Array ParseError) a
toEither = case _ of
  Result _ a -> Right a
  Error pe -> Left $ pure pe
