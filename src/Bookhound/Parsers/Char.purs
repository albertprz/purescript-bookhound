module Bookhound.Parsers.Char where

import Bookhound.FatPrelude

import Bookhound.Parser (Parser, satisfy)
import Bookhound.Parser as Parser
import Bookhound.ParserCombinators (is)
import Control.Alt ((<|>))

anyChar :: Parser Char
anyChar = Parser.anyChar

digit :: Parser Char
digit = satisfy (isDecDigit <<< codePointFromChar) anyChar

hexDigit :: Parser Char
hexDigit = satisfy (isHexDigit <<< codePointFromChar) anyChar

upper :: Parser Char
upper = satisfy (isAsciiUpper <<< codePointFromChar) anyChar

lower :: Parser Char
lower = satisfy (isAsciiLower <<< codePointFromChar) anyChar

alpha :: Parser Char
alpha = lower <|> upper

alphaNum :: Parser Char
alphaNum = alpha <|> digit

space :: Parser Char
space = is ' '

tab :: Parser Char
tab = is '\t'

newLine :: Parser Char
newLine = is '\n'

spaceOrTab :: Parser Char
spaceOrTab = space <|> tab

whiteSpace :: Parser Char
whiteSpace = spaceOrTab <|> newLine

comma :: Parser Char
comma = is ','

dot :: Parser Char
dot = is '.'

colon :: Parser Char
colon = is ':'

quote :: Parser Char
quote = is '\''

doubleQuote :: Parser Char
doubleQuote = is '"'

dash :: Parser Char
dash = is '-'

plus :: Parser Char
plus = is '+'

equal :: Parser Char
equal = is '='

underscore :: Parser Char
underscore = is '_'

hashTag :: Parser Char
hashTag = is '#'

question :: Parser Char
question = is '?'

openParens :: Parser Char
openParens = is '('

closeParens :: Parser Char
closeParens = is ')'

openSquare :: Parser Char
openSquare = is '['

closeSquare :: Parser Char
closeSquare = is ']'

openCurly :: Parser Char
openCurly = is '{'

closeCurly :: Parser Char
closeCurly = is '}'

openAngle :: Parser Char
openAngle = is '<'

closeAngle :: Parser Char
closeAngle = is '>'
