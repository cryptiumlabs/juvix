module Juvix.Core.Parameterisations.All where

import Juvix.Core.Types hiding
  ( apply,
    parseTy,
    parseVal,
    reservedNames,
    reservedOpNames,
    typeOf,
  )
import Juvix.Library hiding ((<|>))
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as Token
import Prelude (String)

-- all primitive types
data AllTy
  = Nat
  | TUnit
  deriving (Show, Eq)

-- c: primitive constant and f: functions
data AllVal
  = Natural Natural -- c (NatTy)
  | Add -- f addition
  | Sub -- f subtraction
  | Mul -- f multiplication
  | Curried NatVal Natural
  | Unit -- c (UnitTy)
  deriving (Show, Eq)

typeOf ∷ AllVal → [AllTy]
typeOf (Natural _) = [Nat]
typeOf (Curried _ _) = [Nat, Nat]
typeOf Add = [Nat, Nat, Nat]
typeOf Sub = [Nat, Nat, Nat]
typeOf Mul = [Nat, Nat, Nat]
typeOf Unit = [TUnit]

apply ∷ AllVal → AllVal → Maybe AllVal
apply Add (Natural x) = pure (Curried Add x)
apply Sub (Natural x) = pure (Curried Sub x)
apply Mul (Natural x) = pure (Curried Mul x)
apply (Curried Add x) (Natural y) = pure (Natural (x + y))
apply (Curried Sub x) (Natural y) = pure (Natural (x - y))
apply (Curried Mul x) (Natural y) = pure (Natural (x * y))
apply _ _ = Nothing

parseTy ∷ Token.GenTokenParser String () Identity → Parser AllTy
parseTy lexer =
  do
    Token.reserved lexer "Nat"
    pure Nat
    <|> do
      Token.reserved lexer "Unit"
      pure TUnit

parseVal ∷ Token.GenTokenParser String () Identity → Parser AllVal
parseVal lexer =
  parseNat lexer <|> parseAdd lexer <|> parseSub lexer <|> parseMul lexer
    <|> do
      Token.reserved lexer "()"
      pure Unit

parseNat ∷ Token.GenTokenParser String () Identity → Parser AllVal
parseNat lexer = Natural . fromIntegral |<< Token.natural lexer

parseAdd ∷ Token.GenTokenParser String () Identity → Parser AllVal
parseAdd lexer = Token.reserved lexer "+" >> pure Add

parseSub ∷ Token.GenTokenParser String () Identity → Parser AllVal
parseSub lexer = Token.reserved lexer "-" >> pure Sub

parseMul ∷ Token.GenTokenParser String () Identity → Parser AllVal
parseMul lexer = Token.reserved lexer "*" >> pure Mul

reservedNames ∷ [String]
reservedNames =
  [ "Nat",
    "+",
    "-",
    "*", --Nat
    "Unit",
    "()" --Unit
  ]

reservedOpNames ∷ [String]
reservedOpNames = []

all ∷ Parameterisation AllTy AllVal
all =
  Parameterisation typeOf apply parseTy parseVal reservedNames reservedOpNames
