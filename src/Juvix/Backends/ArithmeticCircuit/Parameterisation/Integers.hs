module Juvix.Backends.ArithmeticCircuit.Parameterisation.Integers where

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
import Text.Show
import Prelude (String)

-- k: primitive type: naturals
data Ty
  = Ty
  deriving (Show, Eq)

-- c: primitive constant and f: functions
data Val
  = Val Int -- c
  | Add -- f addition
  | Mul -- f multiplication
  | Curried Val Int
  deriving (Eq)

instance Show Val where
  show (Val x) = "Int " <> Text.Show.show x
  show Add = "+"
  show Mul = "*"
  show (Curried x y) = Juvix.Library.show x <> " " <> Text.Show.show y

typeOf ∷ Val → NonEmpty Ty
typeOf (Val _) = Ty :| []
typeOf (Curried _ _) = Ty :| [Ty]
typeOf Add = Ty :| [Ty, Ty]
typeOf Mul = Ty :| [Ty, Ty]

apply ∷ Val → Val → Maybe Val
apply Add (Val x) = pure (Curried Add x)
apply Mul (Val x) = pure (Curried Mul x)
apply (Curried Add x) (Val y) = pure (Val (x + y))
apply (Curried Mul x) (Val y) = pure (Val (x * y))
apply _ _ = Nothing

parseTy ∷ Token.GenTokenParser String () Identity → Parser Ty
parseTy lexer = do
  Token.reserved lexer "Int"
  pure Ty

parseVal ∷ Token.GenTokenParser String () Identity → Parser Val
parseVal lexer =
  parseNat lexer <|> parseAdd lexer <|> parseMul lexer

parseNat ∷ Token.GenTokenParser String () Identity → Parser Val
parseNat lexer = Val . fromIntegral |<< Token.integer lexer

parseAdd ∷ Token.GenTokenParser String () Identity → Parser Val
parseAdd lexer = Token.reserved lexer "+" >> pure Add

parseMul ∷ Token.GenTokenParser String () Identity → Parser Val
parseMul lexer = Token.reserved lexer "*" >> pure Mul

reservedNames ∷ [String]
reservedNames = ["Int", "+", "-", "*"]

reservedOpNames ∷ [String]
reservedOpNames = []

t ∷ Parameterisation Ty Val
t =
  Parameterisation typeOf apply parseTy parseVal reservedNames reservedOpNames
