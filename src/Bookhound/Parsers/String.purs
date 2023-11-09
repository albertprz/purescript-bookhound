module Bookhound.Parsers.String where

import Prelude

import Bookhound.Parser (Parser)
import Bookhound.ParserCombinators (inverse, maybeWithin, maybeWithinBoth, within, withinBoth, (->>-), (|+), (|?), (||*), (||+))
import Bookhound.Parsers.Char (alpha, alphaNum, anyChar, closeAngle, closeCurly, closeParens, closeSquare, digit, doubleQuote, lower, newLine, openAngle, openCurly, openParens, openSquare, quote, space, spaceOrTab, tab, upper, whiteSpace)
import Data.Foldable (fold)

anyString :: Parser String
anyString = (||*) anyChar

word :: Parser String
word = (||+) $ inverse whiteSpace

digits :: Parser String
digits = (||+) digit

uppers :: Parser String
uppers = (||+) upper

lowers :: Parser String
lowers = (||+) lower

alphas :: Parser String
alphas = (||+) alpha

alphaNums :: Parser String
alphaNums = (||+) alphaNum

spaces :: Parser String
spaces = (||+) space

tabs :: Parser String
tabs = (||+) tab

newLines :: Parser String
newLines = (||+) newLine

spacesOrTabs :: Parser String
spacesOrTabs = (||+) spaceOrTab

spacing :: Parser String
spacing = (||+) whiteSpace

blankLine :: Parser String
blankLine = (|?) spacesOrTabs ->>- newLine

blankLines :: Parser String
blankLines = fold <$> (|+) blankLine

withinQuotes :: forall b. Parser b -> Parser b
withinQuotes = within quote

withinDoubleQuotes :: forall b. Parser b -> Parser b
withinDoubleQuotes = within doubleQuote

withinParens :: forall b. Parser b -> Parser b
withinParens = withinBoth openParens closeParens

withinSquareBrackets :: forall b. Parser b -> Parser b
withinSquareBrackets = withinBoth openSquare closeSquare

withinCurlyBrackets :: forall b. Parser b -> Parser b
withinCurlyBrackets = withinBoth openCurly closeCurly

withinAngleBrackets :: forall b. Parser b -> Parser b
withinAngleBrackets = withinBoth openAngle closeAngle

maybeWithinQuotes :: forall b. Parser b -> Parser b
maybeWithinQuotes = maybeWithin quote

maybeWithinDoubleQuotes :: forall b. Parser b -> Parser b
maybeWithinDoubleQuotes = maybeWithin doubleQuote

maybeWithinParens :: forall b. Parser b -> Parser b
maybeWithinParens = maybeWithinBoth openParens closeParens

maybeWithinSquareBrackets :: forall b. Parser b -> Parser b
maybeWithinSquareBrackets = maybeWithinBoth openSquare closeSquare

maybeWithinCurlyBrackets :: forall b. Parser b -> Parser b
maybeWithinCurlyBrackets = maybeWithinBoth openCurly closeCurly

maybeWithinAngleBrackets :: forall b. Parser b -> Parser b
maybeWithinAngleBrackets = maybeWithinBoth openAngle closeAngle
