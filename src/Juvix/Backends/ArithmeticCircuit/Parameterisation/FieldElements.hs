module Juvix.Backends.ArithmeticCircuit.Parameterisation.FieldElements where

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
import Data.Curve.Weierstrass.BN254 (Fr)
import Circuit.Arithmetic (Wire(..))
import Circuit.Expr
import Circuit.Lang

-- k: primitive type: unit
data Ty
  = Ty
  deriving (Show, Eq)

type F = Fr

-- c: primitive constant and f: functions
data Val
  = Val (Expr Wire F F)
  | Add
  | Mul
  | Curried Val (Expr Wire F F)
  deriving (Show)

typeOf ∷ Val → NonEmpty Ty
typeOf (Val _) = Ty :| []
typeOf (Curried _ _) = Ty :| [Ty]
typeOf Add = Ty :| [Ty, Ty]
typeOf Mul = Ty :| [Ty, Ty]

apply ∷ Val → Val → Maybe Val
apply Add (Val x) = pure (Curried Add x)
apply Mul (Val x) = pure (Curried Mul x)
apply (Curried Add x) (Val y) = pure (Val (add x y))
apply (Curried Mul x) (Val y) = pure (Val (mul x y))
apply _ _ = Nothing

parseTy ∷ Token.GenTokenParser String () Identity → Parser Ty
parseTy lexer = do
  Token.reserved lexer "FieldElements"
  pure Ty

parseVal ∷ Token.GenTokenParser String () Identity → Parser Val
parseVal lexer = undefined -- idk yet how to parse it, hexadecimal?

reservedNames ∷ [String]
reservedNames = ["FieldElements", "F", "add", "mul"]

reservedOpNames ∷ [String]
reservedOpNames = []

t ∷ Parameterisation Ty Val
t =
  Parameterisation typeOf apply parseTy parseVal reservedNames reservedOpNames
