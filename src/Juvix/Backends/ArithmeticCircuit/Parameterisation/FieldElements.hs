{-# OPTIONS_GHC -Wwarn=incomplete-patterns #-}

module Juvix.Backends.ArithmeticCircuit.Parameterisation.FieldElements where

import Juvix.Core.Types hiding
  ( apply,
    parseTy,
    parseVal,
    reservedNames,
    reservedOpNames,
    typeOf,
  )
import qualified Juvix.Core.Parameterisation as P
import Juvix.Library hiding ((<|>))
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as Token
import Prelude (String)

data Ty
  = Ty
  deriving (Show, Eq)

class FieldElement (e :: * -> * -> *) where
  size :: e f g -> Integer
  prim :: f -> e f f
  add :: e f f -> e f f -> e f f
  mul :: e f f -> e f f -> e f f
  sub :: e f f -> e f f -> e f f
  neg :: e f f -> e f f
  intExp :: e f f -> e f g -> e f f
  eq :: e f f -> e f f -> e f Bool

data Val f where
  Val :: FieldElement e => e f g -> Val (e f g)
  Add :: FieldElement e => Val (e f g)
  Mul :: FieldElement e => Val (e f g)
  Neg :: FieldElement e => Val (e f g)
  IntExp :: FieldElement e => Val (e f g)
  Eq :: FieldElement e => Val (e f g)
  Curried :: FieldElement e => Val (e f g) -> e f g -> Val (e f g)

typeOf :: Val a -> NonEmpty Ty
typeOf (Val _) = Ty :| []
typeOf (Curried _ _) = Ty :| [Ty]
typeOf Add = Ty :| [Ty, Ty]
typeOf Mul = Ty :| [Ty, Ty]
typeOf Neg = undefined
typeOf IntExp = undefined
typeOf Eq = undefined

apply :: FieldElement e => Val (e f f) -> Val (e f f) -> Maybe (Val (e f f))
apply Add (Val x) = pure (Curried Add x)
apply Mul (Val x) = pure (Curried Mul x)
apply (Curried Add x) (Val y) = pure (Val (add x y))
apply (Curried Mul x) (Val y) = pure (Val (mul x y))
apply _ _ = Nothing

parseTy :: Token.TokenParser () -> Parser Ty
parseTy lexer = do
  Token.reserved lexer "FieldElements"
  pure Ty

parseVal :: Token.TokenParser () -> Parser (Val a)
parseVal lexer = undefined

reservedNames :: [String]
reservedNames = ["FieldElements", "F", "add", "mul"]

reservedOpNames :: [String]
reservedOpNames = []

t :: FieldElement e => Parameterisation Ty (Val (e f f))
t =
  Parameterisation {
    typeOf, apply, parseTy, parseVal, reservedNames, reservedOpNames,
    stringTy = \_ _ -> False,
    stringVal = const Nothing,
    intTy = \_ _ -> False,
    intVal = const Nothing,
    floatTy = \_ _ -> False,
    floatVal = const Nothing
  }
