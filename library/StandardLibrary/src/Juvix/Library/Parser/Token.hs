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
    or,
    equal,
    nequal,
    gequal,
    lequal,
    lesser,
    not,
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

import qualified Data.Set as Set
import qualified GHC.Unicode as Unicode
import Protolude hiding (and, div, hash, not, or)

default (Text)

assign :: Text
assign = "="

under :: Char
under = '_'

space :: Text
space = " "

colon :: Text
colon = ":"

semi :: Text
semi = ";"

comma :: Text
comma = ","

hash :: Text
hash = "#"

backSlash :: Text
backSlash = "\\"

quote :: Text
quote = "\'"

pipe :: Text
pipe = "|"

at :: Text
at = "@"

backtick :: Text
backtick = "`"

newLine :: Text
newLine = "\n"

openParen :: Text
openParen = "("

closeParen :: Text
closeParen = ")"

openCurly :: Text
openCurly = "{"

closeCurly :: Text
closeCurly = "}"

openBracket :: Text
openBracket = "["

closeBracket :: Text
closeBracket = "]"

---------------------
-- Infix Operators --
---------------------

mult :: Text
mult = "*"

add :: Text
add = "+"

sub :: Text
sub = "-"

div :: Text
div = "/"

pow :: Text
pow = "^"

and :: Text
and = "&"

or :: Text
or = "||"

equal :: Text
equal = "=="

nequal :: Text
nequal = "!="

gequal :: Text
gequal = ">="

lequal :: Text
lequal = "<="

greater :: Text
greater = ">"

lesser :: Text
lesser = "<"

---------------------
-- Start Operators --
---------------------

question :: Text
question = "?"

not :: Text
not = "!"

percent :: Text
percent = "%"

dot :: Text
dot = "."

dash :: Text
dash = "-"

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

infixOperators :: [Text]
infixOperators =
  [ mult,
    add,
    sub,
    div,
    pow,
    and,
    or,
    equal,
    nequal,
    gequal,
    lequal,
    greater,
    lesser
  ]

middleOperators :: [Text]
middleOperators =
  [ not,
    dot,
    percent,
    question,
    dash
  ]

operators :: [Text]
operators = infixOperators ++ middleOperators

validStartSymbol :: Char -> Bool
validStartSymbol w =
  Unicode.isAlpha w || w == under

validInfixSymbol :: Text -> Bool
validInfixSymbol = flip elem infixOperators

-- I don't think allowing any symbol is a good idea
-- Unicode.isSymbol w
--   || w == mult
--   || w == dash
--   || w == amper
--   || w == colon
--   || w == div
--   || w == percent
--   || w == dot

validMiddleSymbol :: Text -> Bool
validMiddleSymbol = flip elem middleOperators

validUpperSymbol :: Char -> Bool
validUpperSymbol = Unicode.isUpper

-- TODO: How to deal with digit?
-- validStartSymbol w
--   || digit w
--   || w == dash
--   || w == not
--   || w == question
--   || w == percent
