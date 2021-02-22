module Juvix.Library.Parser.Token
  ( operators,
    reservedSymbols,
    reservedWords,
    assign,
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
    semi,
    colon,
    at,
    percent,
    lparen,
    rparen,
    lbrace,
    rbrace,
    lbracket,
    rbracket,
    rarrow,
  )
where

import qualified Data.Set as Set
import Data.Text (Text)
import Protolude (IsString, Ord, Set)

default (Text)

assign :: Text
assign = "="

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
and = "&&"

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

not :: Text
not = "!"

dot :: Text
dot = "."

semi :: Text
semi = ";"

colon :: Text
colon = ":"

at :: Text
at = "@"

percent :: Text
percent = "%"

lparen :: Text
lparen = "("

rparen :: Text
rparen = ")"

lbrace :: Text
lbrace = "{"

rbrace :: Text
rbrace = "}"

lbracket :: Text
lbracket = "["

rbracket :: Text
rbracket = "]"

rarrow :: Text
rarrow = "->"

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

operators :: [Text]
operators =
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
    lesser,
    not,
    dot,
    percent
  ]
