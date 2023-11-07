module Bookhound.Parser
  ( Parser
  , ParseResult(..)
  , ParseError(..)
  , Input
  , parse
  , runParser
  , andThen
  , exactly
  , both
  , anyOf
  , allOf
  , char
  , withTransform
  , withError
  , withErrorN
  , except
  ) where

import Bookhound.FatPrelude

import Control.Alt (class Alt)
import Control.Alternative (class Alternative, class Plus, empty)
import Control.Apply (lift2)
import Control.Lazy (class Lazy)
import Control.Monad.Error.Class (class MonadThrow)
import Control.MonadPlus (class MonadPlus)
import Data.Foldable (class Foldable)
import Data.Lazy as Lazy
import Data.Set as Set
import Data.String (null) as String
import Data.String.CodeUnits (uncons) as String
import Unsafe.Coerce (unsafeCoerce)

newtype Parser a = P
  { parse :: Input -> ParseResult a
  , transform :: Transform
  , errors :: Set (Int /\ ParseError)
  }

instance Functor Parser where
  map f (P { parse: p, transform: (t :: Transform), errors: e }) =
    applyTransformError t e $ mkParser (map f <<< p)

instance Apply Parser where
  apply
    (P { parse: p, transform: (t :: Transform), errors: e })
    (P { parse: p', transform: (t' :: Transform), errors: e' }) =
    applyTransformsErrors [ t, t' ] [ e, e' ] $ mkParser
      \x -> case p x of
        Error pe -> Error pe
        Result i f -> case p' i of
          Error pe -> Error pe
          Result i' a -> Result i' (f a)

instance Applicative Parser where
  pure a = mkParser (flip Result a)

instance Bind Parser where
  bind (P { parse: p, transform: (t :: Transform), errors: e }) f =
    applyTransformError t e $ mkParser
      \x -> case p x of
        Result i a -> parse (f a) i
        Error pe -> Error pe

instance Monad Parser

instance Semigroup a => Semigroup (Parser a) where
  append = lift2 append

instance Monoid a => Monoid (Parser a) where
  mempty = pure mempty

instance Alt Parser where
  alt
    (P { parse: p, transform: (t :: Transform), errors: e })
    (P { parse: p', transform: (t' :: Transform), errors: e' }) =
    applyTransformsErrors [ t, t' ] [ e, e' ] $
      mkParser
        \x -> case p x of
          Error _ -> p' x
          result -> result

instance Plus Parser where
  empty = mkParser \i ->
    if String.null i then
      Error UnexpectedEof
    else
      Error $ ExpectedEof i

instance Alternative Parser

instance MonadPlus Parser

instance MonadThrow ParseError Parser where
  throwError = mkParser <<< const <<< Error

instance Lazy (Parser a) where
  defer f = mkParser \x -> parse (Lazy.force lazy) x
    where
    lazy = Lazy.defer f

char :: Parser Char
char = mkParser
  $ maybe (Error UnexpectedEof) (\x -> Result x.tail x.head)
  <<< String.uncons

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

andThen :: forall a. Parser String -> Parser a -> Parser a
andThen p1 (p2@(P { transform: (t :: Transform), errors: e })) =
  applyTransformError t e $
    mkParser (\i -> parse p2 $ fromRight i $ runParser p1 i)

exactly :: forall a. Parser a -> Parser a
exactly (P { parse: p, transform: (t :: Transform), errors: e }) =
  applyTransformError t e $ mkParser
    \x -> case p x of
      result@(Result i _) | i == mempty -> result
      Result i _ -> Error $ ExpectedEof i
      err -> err

anyOf :: forall f a. Foldable f => f (Parser a) -> Parser a
anyOf = foldl alt empty

allOf :: forall f a. Foldable f => f (Parser a) -> Parser a
allOf xs
  | hasNone xs = empty
  | otherwise = foldl both (pure $ unsafeCoerce unit) xs

both :: forall a. Parser a -> Parser a -> Parser a
both
  (P { parse: p, transform: (t :: Transform), errors: e })
  (P { parse: p', transform: (t' :: Transform), errors: e' }) =
  applyTransformsErrors [ t, t' ] [ e, e' ] $
    mkParser
      \x -> case p x of
        Result _ _ -> p' x
        err -> err

except :: forall a. Parser a -> Parser a -> Parser a
except
  (P { parse: p, transform: (t :: Transform), errors: e })
  (P { parse: p' }) =
  applyTransformError t e $ mkParser
    \x -> case p' x of
      Result _ _ -> Error (ExpectedEof x)
      Error _ -> p x

withError :: forall a. String -> Parser a -> Parser a
withError = withErrorN 0

withErrorN :: forall a. Int -> String -> Parser a -> Parser a
withErrorN n str = applyError $ Set.singleton (n /\ ErrorAt str)

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

data ParseResult a
  = Result Input a
  | Error ParseError

derive instance Eq a => Eq (ParseResult a)

instance (Show a) => Show (ParseResult a) where
  show (Result i a) =
    "Pending: " <> " >" <> i <> "< " <> "\n\nResult: \n" <> show a
  show (Error err) = show err

instance Functor ParseResult where
  map f (Result i a) = Result i (f a)
  map _ (Error pe) = Error pe

data ParseError
  = UnexpectedEof
  | ExpectedEof Input
  | ErrorAt String

derive instance Eq ParseError
derive instance Ord ParseError

instance Show ParseError where
  show (UnexpectedEof) =
    "Unexpected end of stream"
  show (ExpectedEof i) =
    "Expected end of stream, but got " <> ">" <> i <> "<"
  show (ErrorAt s) =
    "Error at " <> s

type Input = String

type Transform = forall b. Maybe (Parser b -> Parser b)
