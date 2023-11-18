module Bookhound.Parsers.String (anyString, word, digits, uppers, lowers, alphas, alphaNums, spaces, tabs, newLines, spacesOrTabs, spacing, blankLine, blankLines, betweenQuotes, betweenDoubleQuotes, betweenParens, betweenSquare, betweenCurly, betweenAngle, maybeBetweenQuotes, maybeBetweenDoubleQuotes, maybeBetweenParens, maybeBetweenSquare, maybeBetweenCurly, maybeBetweenAngle) where

import Prelude hiding (between)

import Bookhound.Parser (Parser)
import Bookhound.ParserCombinators (inverse, maybeBetween, maybeSurroundedBy, between, surroundedBy, (->>-), (|+), (|?), (||*), (||+))
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

betweenQuotes :: forall b. Parser b -> Parser b
betweenQuotes = between quote

betweenDoubleQuotes :: forall b. Parser b -> Parser b
betweenDoubleQuotes = between doubleQuote

betweenParens :: forall b. Parser b -> Parser b
betweenParens = surroundedBy openParens closeParens

betweenSquare :: forall b. Parser b -> Parser b
betweenSquare = surroundedBy openSquare closeSquare

betweenCurly :: forall b. Parser b -> Parser b
betweenCurly = surroundedBy openCurly closeCurly

betweenAngle :: forall b. Parser b -> Parser b
betweenAngle = surroundedBy openAngle closeAngle

maybeBetweenQuotes :: forall b. Parser b -> Parser b
maybeBetweenQuotes = maybeBetween quote

maybeBetweenDoubleQuotes :: forall b. Parser b -> Parser b
maybeBetweenDoubleQuotes = maybeBetween doubleQuote

maybeBetweenParens :: forall b. Parser b -> Parser b
maybeBetweenParens = maybeSurroundedBy openParens closeParens

maybeBetweenSquare :: forall b. Parser b -> Parser b
maybeBetweenSquare = maybeSurroundedBy openSquare closeSquare

maybeBetweenCurly :: forall b. Parser b -> Parser b
maybeBetweenCurly = maybeSurroundedBy openCurly closeCurly

maybeBetweenAngle :: forall b. Parser b -> Parser b
maybeBetweenAngle = maybeSurroundedBy openAngle closeAngle
