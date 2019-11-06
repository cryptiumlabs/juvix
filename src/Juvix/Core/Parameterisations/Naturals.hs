module Juvix.Core.Parameterisations.Naturals where

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

-- k: primitive type: naturals
data NatTy
  = Nat
  deriving (Show, Eq)

-- c: primitive constant and f: functions
data NatVal
  = Natural Natural -- c
  | Add -- f addition
  | Sub -- f subtraction
  | Mul -- f multiplication
  | Curried NatVal Natural
  deriving (Show, Eq)

typeOf ∷ ∀ a. ([NatTy] → a) → NatVal → Either a NatTy
typeOf _ (Natural _) = Right Nat
typeOf arrow (Curried _ _) = Left $ arrow [Nat, Nat]
typeOf arrow Add = Left $ arrow [Nat, Nat, Nat]
typeOf arrow Sub = Left $ arrow [Nat, Nat, Nat]
typeOf arrow Mul = Left $ arrow [Nat, Nat, Nat]

apply ∷ NatVal → NatVal → Maybe NatVal
apply Add (Natural x) = pure (Curried Add x)
apply Sub (Natural x) = pure (Curried Sub x)
apply Mul (Natural x) = pure (Curried Mul x)
apply (Curried Add x) (Natural y) = pure (Natural (x + y))
apply (Curried Sub x) (Natural y) = pure (Natural (x - y))
apply (Curried Mul x) (Natural y) = pure (Natural (x * y))
apply _ _ = Nothing

parseTy ∷ Token.GenTokenParser String () Identity → Parser NatTy
parseTy lexer = do
  Token.reserved lexer "Nat"
  pure Nat

parseVal ∷ Token.GenTokenParser String () Identity → Parser NatVal
parseVal lexer =
  parseNat lexer <|> parseAdd lexer <|> parseSub lexer <|> parseMul lexer

parseNat ∷ Token.GenTokenParser String () Identity → Parser NatVal
parseNat lexer = Natural . fromIntegral |<< Token.natural lexer

parseAdd ∷ Token.GenTokenParser String () Identity → Parser NatVal
parseAdd lexer = Token.reserved lexer "+" >> pure Add

parseSub ∷ Token.GenTokenParser String () Identity → Parser NatVal
parseSub lexer = Token.reserved lexer "-" >> pure Sub

parseMul ∷ Token.GenTokenParser String () Identity → Parser NatVal
parseMul lexer = Token.reserved lexer "*" >> pure Mul

reservedNames ∷ [String]
reservedNames = ["Nat", "+", "-", "*"]

reservedOpNames ∷ [String]
reservedOpNames = []

nat ∷ Parameterisation NatTy NatVal
nat =
  Parameterisation typeOf apply parseTy parseVal reservedNames reservedOpNames
