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

data Ty
  = Ty
  deriving (Show, Eq)

class FieldElement f where
  add :: f -> f -> f
  mul :: f -> f -> f
  sub :: f -> f -> f
  neg :: f -> f
  intExp :: f -> f -> f
  eq :: f -> f -> f

data Val f where
  Val :: FieldElement f => f -> Val f
  Add :: Val f
  Mul :: Val f
  Curried :: FieldElement f => Val f -> f -> Val f

typeOf ∷ Val a → NonEmpty Ty
typeOf (Val _) = Ty :| []
typeOf (Curried _ _) = Ty :| [Ty]
typeOf Add = Ty :| [Ty, Ty]
typeOf Mul = Ty :| [Ty, Ty]

apply ∷ FieldElement f => Val f → Val f → Maybe (Val f)
apply Add (Val x) = pure (Curried Add x)
apply Mul (Val x) = pure (Curried Mul x)
apply (Curried Add x) (Val y) = pure (Val (add x y))
apply (Curried Mul x) (Val y) = pure (Val (mul x y))
apply _ _ = Nothing

parseTy ∷ Token.GenTokenParser String () Identity → Parser Ty
parseTy lexer = do
  Token.reserved lexer "FieldElements"
  pure Ty

parseVal ∷ Token.GenTokenParser String () Identity → Parser (Val a)
parseVal lexer = undefined -- idk yet how to parse it, hexadecimal?

reservedNames ∷ [String]
reservedNames = ["FieldElements", "F", "add", "mul"]

reservedOpNames ∷ [String]
reservedOpNames = []

t ∷ FieldElement f => Parameterisation Ty (Val f)
t =
  Parameterisation typeOf apply parseTy parseVal reservedNames reservedOpNames
