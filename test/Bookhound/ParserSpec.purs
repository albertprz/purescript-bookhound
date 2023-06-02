module Bookhound.ParserSpec where

import FatPrelude hiding (length, toUpper)

import Bookhound.Parser (ParseError(..), ParseResult(..), Parser, allOf, anyOf, char, check, errorParser, exactly, except, isMatch, parse, runParser, withErrorN, withTransform)
import Data.Array (length)
import Data.Array.NonEmpty (cons')
import Data.CodePoint.Unicode (toUpper)
import Data.List (toUnfoldable) as List
import Test.QuickCheck ((===))
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.QuickCheck.Gen (Gen, arrayOf, oneOf)
import Test.Spec (Spec, describe, it)
import Test.Spec.QuickCheck (quickCheck)

spec :: Spec Unit
spec = describe "Bookhound.Parser" $ do

  describe "Functor laws" $ do

    it "Identity" $ quickCheck
      \x -> parse (identity <$> char) x
        === parse char x

    it "Composition" $ quickCheck
      \x -> parse ((all isLower <<< toUpper) <$> codePoint) x
        ===
          parse ((all isLower <$> _) <<< (toUpper <$> _) $ codePoint) x

  describe "Applicative laws" $ do

    it "Identity" $ quickCheck
      \x -> parse (pure identity <*> char) x
        ===
          parse char x

    it "Homomorphism" $ quickCheck
      \x (y :: Array Int) -> parse (pure sum <*> pure y) x
        ===
          parse (pure (sum y)) x

    it "Interchange" $ quickCheck
      \x (y :: Array Int) -> parse (pure length <*> pure y) x
        ===
          parse (pure (_ $ y) <*> pure length) x

  describe "Monad laws" $ do

    it "Left identity" $ quickCheck
      \x -> parse (pure 'a' >>= isMatch (==) char) x
        ===
          parse (isMatch (==) char 'a') x

    it "Right identity" $ quickCheck
      \x -> parse (char >>= pure) x
        ===
          parse char x

    it "Associativity" $ quickCheck
      \x ->
        parse
          (char >>= (\y -> isMatch (<) char y >>= isMatch (>) char))
          x
          ===
            parse ((char >>= isMatch (<) char) >>= isMatch (>) char) x

  describe "char"
    $ it "parses a single char"
    $ quickCheck
        \x -> parse char x
          ===
            case unpack x of
              (ch : rest) -> Result (pack rest) ch
              Nil -> Error UnexpectedEof

  describe "isMatch" $ do

    it "works for ==" $ quickCheck
      \x y -> parse (isMatch (==) char y) x
        ===
          case unpack x of
            (ch : rest)
              | ch == y -> Result (pack rest) ch
              | otherwise -> Error $ UnexpectedChar ch
            Nil -> Error UnexpectedEof

    it "works for /=" $ quickCheck
      \x y -> parse (isMatch (/=) char y) x
        ===
          case unpack x of
            (ch : rest)
              | ch /= y -> Result (pack rest) ch
              | otherwise -> Error $ UnexpectedChar ch
            Nil -> Error UnexpectedEof

  describe "check"
    $ it "performs a check on the parse result"
    $ quickCheck
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
    $ it "the first parser then runs"
    $ quickCheck
        \x -> parse (except char (char *> char)) x
          ===
            case unpack x of
              (ch : Nil) -> Result "" ch
              (_ : _) -> Error $ NoMatch "except"
              Nil -> Error UnexpectedEof

  describe "anyOf"
    $ it "returns first parser success or last parser error if all fail"
    $ quickCheck
    $
      \(x :: AlphaNumString) ->
        parse
          ( anyOf
              [ errorParser $ ErrorAt "firstError"
              , "firstSuccess" <$ char
              , "secondSuccess" <$ char
              , errorParser $ ErrorAt "secondError"
              , errorParser $ ErrorAt "lastError"
              ]
          )
          (unwrap x)
          ===
            case unpack $ unwrap x of
              (_ : rest) -> Result (pack rest) "firstSuccess"
              Nil -> Error $ ErrorAt "lastError"

  describe "allOf" $ do

    describe "when any parser fails"
      $ it "returns first parser error"
      $ quickCheck
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

  describe "when all parsers succeed" $
    it "returns last parser success" $ quickCheck
      \x -> parse (allOf ["firstSuccess"  <$ char,
                         "secondSuccess" <$ char,
                         "thirdSuccess"  <$ char]) x
           ===
           case unpack x of
             (_ : rest) -> Result (pack rest) "thirdSuccess"
             Nil         -> Error UnexpectedEof

  describe "exactly" $

    it "returns an error when some chars remain after parsing" $
      quickCheck
      \x -> parse (exactly char) x
          ===
          case unpack x of
            (ch : Nil)       -> Result "" ch
            (_ : rest) -> Error $ ExpectedEof (pack rest)
            Nil         -> Error UnexpectedEof

  describe "runParser" $

    it "parses exactly and wraps the result into an either" $ quickCheck
      \x -> runParser char x
          ===
          toEither (parse (exactly char) x)

  describe "withError" $

    it "includes error labels in order when running the parser" $
      quickCheck
      \x -> runParser ((/\) <$> withErrorN 2 "firstErr" char
                          <*> withErrorN 1 "secondErr" char) x
          ===
          case unpack x of
            (ch1 : ch2 : Nil) -> Right (ch1 /\ ch2)
            (_ : _ : rest)   -> Left [ErrorAt "firstErr",
                                      ErrorAt "secondErr",
                                      ExpectedEof (pack rest)]
            _                -> Left [ErrorAt "firstErr",
                                      ErrorAt "secondErr",
                                      UnexpectedEof]

  describe "withTransform" $

    it "transforms current parser with provided fn" $ quickCheck
      \x -> parse (withTransform (\p -> char *> p <* char) char) x
          ===
          parse (char *> char <* char) x



pack :: List Char -> String
pack = fromCharArray <<< List.toUnfoldable

unpack :: String -> List Char
unpack = toUnfoldable <<< toCharArray

codePoint :: Parser CodePoint
codePoint = codePointFromChar <$> char

toEither :: forall a. ParseResult a -> Either (Array ParseError) a
toEither = case _ of
  Result _ a -> Right a
  Error pe   -> Left $ pure pe



newtype AlphaNumString = AlphaNumString String

derive instance Newtype AlphaNumString _
derive newtype instance Eq AlphaNumString
derive newtype instance Ord AlphaNumString

instance Arbitrary AlphaNumString where
  arbitrary = AlphaNumString <<< fromCharArray <$> arrayOf anyChar

anyChar :: Gen Char
anyChar = oneOf $ pure <$> cons' 'a' (toCharArray "bcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")

