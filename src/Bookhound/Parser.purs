module Bookhound.Parser
  ( Parser
  , ParseResult(..)
  , ParseError(..)
  , Input
  , parse
  , runParser
  , errorParser
  , andThen
  , exactly
  , isMatch
  , check
  , anyOf
  , allOf
  , char
  , withTransform
  , withError
  , withErrorN
  , except
  ) where

import Bookhound.FatPrelude

import Control.Lazy (class Lazy, defer)
import Data.Lazy as Lazy
import Data.Set as Set

type Input = String

newtype Parser a = P
  { parse :: Input -> ParseResult a
  , transform :: Transform
  , errors :: Set (Int /\ ParseError)
  }

type Transform = forall b. Maybe (Parser b -> Parser b)

data ParseResult a
  = Result Input a
  | Error ParseError

derive instance Eq a => Eq (ParseResult a)

data ParseError
  = UnexpectedEof
  | ExpectedEof Input
  | UnexpectedChar Char
  | UnexpectedString String
  | NoMatch String
  | ErrorAt String

derive instance Eq ParseError
derive instance Ord ParseError

instance (Show a) => Show (ParseResult a) where
  show (Result i a) =
    "Pending: " <> " >" <> i <> "< " <> "\n\nResult: \n" <> show a
  show (Error err) = show err

instance Show ParseError where
  show (UnexpectedEof) = "Unexpected end of stream"
  show (ExpectedEof i) =
    "Expected end of stream, but got " <> ">" <> i <> "<"
  show (UnexpectedChar c) = "Unexpected char: " <> "[" <> show c <> "]"
  show (UnexpectedString s) = "Unexpected string: " <> "[" <> s <> "]"
  show (NoMatch s) = "Did not match condition: " <> s
  show (ErrorAt s) = "Error at " <> s

instance Functor ParseResult where
  map f (Result i a) = Result i (f a)
  map _ (Error pe) = Error pe

instance Functor Parser where
  map f (P { parse: p, transform: (t :: Transform), errors: e }) =
    applyTransformError t e $ mkParser (map f <<< p)

instance Apply Parser where
  apply
    (P { parse: p, transform: (t :: Transform), errors: e })
    (P { parse: p', transform: (t' :: Transform), errors: e' }) =
    applyTransformsErrors [ t, t' ] [ e, e' ] $ mkParser
      ( \x -> case p x of
          Error pe -> Error pe
          Result i f -> case p' i of
            Error pe -> Error pe
            Result i' a -> Result i' (f a)
      )

instance Applicative Parser where
  pure a = mkParser (_ `Result` a)

instance Bind Parser where
  bind (P { parse: p, transform: (t :: Transform), errors: e }) f =
    applyTransformError t e $ mkParser
      ( \x -> case p x of
          Result i a -> parse (f a) i
          Error pe -> Error pe
      )

instance Lazy (Parser a) where
  defer f = mkParser \x -> parse (Lazy.force lazy) x
    where
    lazy = Lazy.defer f

parse :: forall a. Parser a -> Input -> ParseResult a
parse (P x) = x.parse

runParser :: forall a. Parser a -> Input -> Either (Array ParseError) a
runParser (p@(P { errors: e })) i =
  toEither $ parse (exactly p) i
  where
  toEither = case _ of
    Result _ a -> Right a
    Error pe -> Left $ filter hasPriorityError [ pe ]
      <> (snd <$> reverse (Set.toUnfoldable e))
      <> filter (not <<< hasPriorityError) [ pe ]

hasPriorityError :: ParseError -> Boolean
hasPriorityError (ErrorAt _) = true

hasPriorityError _ = false

errorParser :: forall a. ParseError -> Parser a
errorParser = mkParser <<< const <<< Error

andThen :: forall a. Parser String -> Parser a -> Parser a
andThen p1 (p2@(P { transform: (t :: Transform), errors: e })) =
  applyTransformError t e $
    mkParser (\i -> parse p2 $ fromRight i $ runParser p1 i)

char :: Parser Char
char = mkParser
  $ maybe (Error UnexpectedEof)
      (\x -> Result (fromCharArray x.tail) x.head)
  <<< uncons
  <<< toCharArray

exactly :: forall a. Parser a -> Parser a
exactly (P { parse: p, transform: (t :: Transform), errors: e }) =
  applyTransformError t e $ mkParser
    ( \x -> case p x of
        result@(Result i _) | i == mempty -> result
        Result i _ -> Error $ ExpectedEof i
        err -> err
    )

anyOf :: forall a. Array (Parser a) -> Parser a
anyOf ps = anyOfHelper (toUnfoldable ps) Nothing mempty

allOf :: forall a. Array (Parser a) -> Parser a
allOf ps = allOfHelper (toUnfoldable ps) Nothing mempty

anyOfHelper
  :: forall a
   . List (Parser a)
  -> Transform
  -> Set (Int /\ ParseError)
  -> Parser a
anyOfHelper Nil _ _ = errorParser $ NoMatch "anyOf"
anyOfHelper (p : Nil) _ _ = p
anyOfHelper
  (P { parse: p, transform: (t :: Transform), errors: e } : rest)
  t'
  e' =
  applyTransformsErrors [ t, t' ] [ e, e' ] $
    mkParser
      ( \x -> case p x of
          Error _ -> parse (anyOfHelper rest t e) x
          result -> result
      )

allOfHelper
  :: forall a
   . List (Parser a)
  -> Transform
  -> Set (Int /\ ParseError)
  -> Parser a
allOfHelper Nil _ _ = errorParser $ NoMatch "allOf"
allOfHelper (p : Nil) _ _ = p
allOfHelper
  (P { parse: p, transform: (t :: Transform), errors: e } : rest)
  t'
  e' =
  applyTransformsErrors [ t, t' ] [ e, e' ] $ mkParser
    ( \x -> case p x of
        Result _ _ -> parse (allOfHelper rest t e) x
        err -> err
    )

isMatch :: (Char -> Char -> Boolean) -> Parser Char -> Char -> Parser Char
isMatch cond parser c1 = do
  c2 <- parser
  if cond c1 c2 then
    pure c2
  else
    errorParser $ UnexpectedChar c2

check :: forall a. String -> (a -> Boolean) -> Parser a -> Parser a
check condName cond parser = do
  c2 <- parser
  if cond c2 then
    pure c2
  else
    errorParser $ NoMatch condName

except :: forall a. Parser a -> Parser a -> Parser a
except
  (P { parse: p, transform: (t :: Transform), errors: e })
  (P { parse: p' }) =
  applyTransformError t e $ mkParser
    ( \x -> case p' x of
        Result _ _ -> Error $ NoMatch "except"
        Error _ -> p x
    )

withError :: forall a. String -> Parser a -> Parser a
withError = withErrorN 0

withErrorN :: forall a. Int -> String -> Parser a -> Parser a
withErrorN n str = applyError <<< Set.singleton $ (n /\ ErrorAt str)

withTransform :: forall a. (forall b. Parser b -> Parser b) -> Parser a -> Parser a
withTransform t = applyTransform (Just t)

applyTransformsErrors
  :: forall a
   . (forall b. Array (Maybe (Parser b -> Parser b)))
  -> Array (Set (Int /\ ParseError))
  -> Parser a
  -> Parser a
applyTransformsErrors ts es = applyTransformError (findJust ts) (fold es)

applyTransformError
  :: forall a
   . Transform
  -> Set (Int /\ ParseError)
  -> Parser a
  -> Parser a
applyTransformError t e = applyTransform t <<< applyError e

applyTransform :: forall a. Transform -> Parser a -> Parser a
applyTransform f p =
  maybe p (\f' -> P ((extract $ f' p) { transform = f })) f
  where
  extract (P x) = x

applyError :: forall a. Set (Int /\ ParseError) -> Parser a -> Parser a
applyError e (P x) = P (x { errors = e <> x.errors })

mkParser :: forall a. (Input -> ParseResult a) -> Parser a
mkParser p = P { parse: p, transform: Nothing, errors: mempty }
