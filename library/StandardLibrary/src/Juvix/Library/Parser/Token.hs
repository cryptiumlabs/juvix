module Juvix.Library.Parser.Token
  ( infixOperators,
    middleOperators,
    operators,
    reservedWords,
    reservedSymbols,
    validStartSymbol,
    validInfixSymbol,
    validMiddleSymbol,
    validUpperSymbol,
    -- Operators
    mult,
    add,
    sub,
    div,
    pow,
    and,
    lesser,
    bang,
    dot,
    percent,
    -- Other symbols
    semi,
    colon,
    at,
    openParen,
    closeParen,
    openCurly,
    closeCurly,
    openBracket,
    closeBracket,
    assign,
    dash,
    under,
    space,
    comma,
    hash,
    backSlash,
    quote,
    pipe,
    question,
    backtick,
    newLine,
  )
where

import qualified Data.Char as Char
import qualified Data.Set as Set
import qualified GHC.Unicode as Unicode
import Protolude hiding (and, div, hash, not, or)

default (Text)

assign :: Char
assign = '='

under :: Char
under = '_'

space :: Char
space = ' '

colon :: Char
colon = ':'

semi :: Char
semi = ';'

comma :: Char
comma = ','

hash :: Char
hash = '#'

backSlash :: Char
backSlash = '\\'

quote :: Char
quote = '\''

pipe :: Char
pipe = '|'

at :: Char
at = '@'

backtick :: Char
backtick = '`'

newLine :: Char
newLine = '\n'

openParen :: Char
openParen = '('

closeParen :: Char
closeParen = ')'

openCurly :: Char
openCurly = '{'

closeCurly :: Char
closeCurly = '}'

openBracket :: Char
openBracket = '['

closeBracket :: Char
closeBracket = ']'

---------------------
-- Infix Operators --
---------------------

mult :: Char
mult = '*'

add :: Char
add = '+'

sub :: Char
sub = '-'

div :: Char
div = '/'

pow :: Char
pow = '^'

and :: Char
and = '&'

-- or :: Text
-- or = "||"

-- equal :: Text
-- equal = "=="

-- nequal :: Text
-- nequal = "!="

-- gequal :: Text
-- gequal = ">="

-- lequal :: Text
-- lequal = "<="

greater :: Char
greater = '>'

lesser :: Char
lesser = '<'

---------------------
-- Start Operators --
---------------------

question :: Char
question = '?'

bang :: Char
bang = '!'

percent :: Char
percent = '%'

dot :: Char
dot = '.'

dash :: Char
dash = '-'

reservedWords :: (Ord a, IsString a) => Set a
reservedWords =
  Set.fromList
    [ "let",
      "val",
      "type",
      "case",
      "in",
      "open",
      "if",
      "cond",
      "end",
      "of",
      "begin",
      "sig",
      "mod",
      "declare",
      "where"
    ]

reservedSymbols :: (Ord a, IsString a) => Set a
reservedSymbols =
  Set.fromList
    ["=", "|", "", "--"]

infixOperators :: [Char]
infixOperators =
  [ mult,
    add,
    sub,
    div,
    pow,
    and,
    greater,
    lesser
  ]

middleOperators :: [Char]
middleOperators =
  [ bang,
    -- dot,
    percent,
    question,
    dash
  ]

operators :: [Char]
operators = infixOperators ++ middleOperators

validStartSymbol :: Char -> Bool
validStartSymbol w =
  Unicode.isAlpha w
    || w == under

validInfixSymbol :: Char -> Bool
validInfixSymbol w =
  -- elem c infixOperators || Unicode.isSymbol c
  Unicode.isSymbol w
    || w == mult
    || w == dash
    || w == and
    || w == colon
    || w == div
    || w == percent
    || w == dot

validMiddleSymbol :: Char -> Bool
validMiddleSymbol w =
  validStartSymbol w
    || Char.isDigit w
    || w == dash
    || w == bang
    || w == question
    || w == percent

validUpperSymbol :: Char -> Bool
validUpperSymbol = Unicode.isUpper

-- TODO: How to deal with digit?
-- validStartSymbol w
--   || digit w
--   || w == dash
--   || w == bang
--   || w == question
--   || w == percent
