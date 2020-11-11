{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wwarn=incomplete-patterns #-}

module Juvix.Backends.ArithmeticCircuit.Parameterisation.Integers where

import qualified Juvix.Backends.ArithmeticCircuit.Parameterisation.FieldElements as FieldElements
import qualified Juvix.Core.Parameterisation as P
import Juvix.Library hiding ((<|>))
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as Token
import Prelude (String)

-- k: primitive type: naturals
data Ty
  = Ty
  deriving (Show, Eq)

class FieldElements.FieldElement e => FInteger (e :: * -> * -> *) i | i -> e where
  prim :: e f i
  add :: e f i -> e f i -> e f i
  mul :: e f i -> e f i -> e f i
  sub :: e f i -> e f i -> e f i
  neg :: e f i -> e f i
  exp :: e f i -> e f i -> e f f
  eq :: e f i -> e f i -> e f g

type FieldT e i = (FieldElements.FieldElement e, FInteger e i)

-- c: primitive constant and f: functions
data Val f i where
  Val :: FieldT e i => e f i -> Val (e f i) i
  Add :: FieldT e i => Val (e f i) i
  Mul :: FieldT e i => Val (e f i) i
  Neg :: FieldT e i => Val (e f i) i
  Exp :: FieldT e i => Val (e f i) i
  Eq :: FieldT e i => Val (e f i) i
  Curried :: FieldT e i => Val (e f i) i -> e f i -> Val (e f b) i

typeOf :: Val f i -> P.PrimType Ty
typeOf (Val _) = Ty :| []
typeOf (Curried _ _) = Ty :| [Ty]
typeOf Add = Ty :| [Ty, Ty]
typeOf Mul = Ty :| [Ty, Ty]
typeOf Exp = undefined
typeOf Neg = undefined
typeOf Eq = undefined

hasType :: Val f i -> P.PrimType Ty -> Bool
hasType x ty = ty == typeOf x

arity :: Val f i -> Int
arity = pred . length . typeOf

apply :: FieldT e i => Val (e f i) i -> Val (e f i) i -> Maybe (Val (e f i) i)
apply Add (Val x) = pure (Curried Add x)
apply Mul (Val x) = pure (Curried Mul x)
apply (Curried Add x) (Val y) = pure (Val (add x y))
apply (Curried Mul x) (Val y) = pure (Val (mul x y))
apply _ _ = Nothing

parseTy :: Token.TokenParser () -> Parser Ty
parseTy lexer = do
  Token.reserved lexer "Int"
  pure Ty

parseVal :: FieldT e i => Token.TokenParser () -> Parser (Val (e f i) i)
parseVal lexer =
  parseNat lexer <|> parseAdd lexer <|> parseMul lexer

parseNat :: FieldT e i => Token.TokenParser () -> Parser (Val (e f i) i)
parseNat lexer = Val . fromPrim |<< Token.integer lexer
  where
    fromPrim = undefined

parseAdd :: FieldT e i => Token.TokenParser () -> Parser (Val (e f i) i)
parseAdd lexer = Token.reserved lexer "+" >> pure Add

parseMul :: FieldT e i => Token.TokenParser () -> Parser (Val (e f i) i)
parseMul lexer = Token.reserved lexer "*" >> pure Mul

reservedNames :: [String]
reservedNames = ["Int", "+", "-", "*"]

reservedOpNames :: [String]
reservedOpNames = []

builtinTypes :: P.Builtins Ty
builtinTypes = [] -- FIXME

builtinValues :: FieldT e i => P.Builtins (Val (e f i) i)
builtinValues = [] -- FIXME

t :: FieldT e i => P.Parameterisation Ty (Val (e f i) i)
t =
  P.Parameterisation
    { hasType,
      builtinTypes,
      builtinValues,
      parseTy,
      parseVal,
      reservedNames,
      reservedOpNames,
      stringTy = \_ _ -> False,
      stringVal = const Nothing,
      intTy = \i _ -> False, -- TODO
      intVal = const Nothing, -- TODO
      floatTy = \_ _ -> False,
      floatVal = const Nothing
    }
