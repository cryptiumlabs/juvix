{-# OPTIONS_GHC -Wwarn=incomplete-patterns -Wwarn=missing-methods #-}

module Juvix.Backends.ArithmeticCircuit.Parameterisation where

import qualified Circuit.Arithmetic as Arith
import qualified Circuit.Expr as Expr
import qualified Circuit.Lang as Lang
import Data.Pairing.BN254 (Fr)
import qualified Juvix.Backends.ArithmeticCircuit.Parameterisation.Booleans as Booleans
import qualified Juvix.Backends.ArithmeticCircuit.Parameterisation.FieldElements as FieldElements
import qualified Juvix.Backends.ArithmeticCircuit.Parameterisation.Integers as FEInteger
import qualified Juvix.Core.Parameterisation as P
import Juvix.Core.Types hiding
  ( apply,
    parseTy,
    parseVal,
    reservedNames,
    reservedOpNames,
    hasType,
    arity,
  )
import Juvix.Library hiding ((<|>))
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as Token
import Prelude (String)

-- all primitive types
data Ty
  = FETy FieldElements.Ty
  | BoolTy Booleans.Ty
  | FEIntTy FEInteger.Ty
  | Ty
  deriving (Show, Eq)

type F = Fr

type BooleanVal = Booleans.Val (Expr.Expr Arith.Wire F Bool) Bool

type FEVal = FieldElements.Val (Expr.Expr Arith.Wire F F)

type FEInteger = FEInteger.Val (Expr.Expr Arith.Wire F Int) Int

instance FieldElements.FieldElement (Expr.Expr Arith.Wire) where
  prim = Lang.c
  add = Lang.add
  mul = Lang.mul
  sub = Lang.sub
  neg = Expr.EUnOp Expr.UNeg
  eq = Lang.eq
  size _ = 254
  intExp = undefined

instance Booleans.Boolean (Expr.Expr Arith.Wire) Bool where
  and' = Lang.and_
  or' = Lang.or_
  not' = Lang.not_
  true = Expr.EConstBool True
  false = Expr.EConstBool False

-- instance FEInteger.FInteger (Expr.Expr Arith.Wire) Int where
--   prim = Lang.c . toF
--   add x y = Lang.add (toF x) (toF y)
--   mul x y= Lang.mul (toF x) (toF y)
--   sub x y = Lang.sub (toF x) (toF y)
--   neg x y = (Expr.EUnOp Expr.UNeg) . toF
--   eq  x y = Lang.eq (toF x) (toF y)

toF :: Int -> F
toF = undefined

data Val where
  FEVal :: FEVal -> Val
  BoolVal :: BooleanVal -> Val
  IntegerVal :: FEInteger -> Val
  Eq :: Val
  Curried :: Val -> FEVal -> Val

boolTyToAll :: Booleans.Ty -> Ty
boolTyToAll = BoolTy

boolValToAll :: BooleanVal -> Val
boolValToAll = BoolVal

feTyToAll :: FieldElements.Ty -> Ty
feTyToAll = FETy

feValToAll :: FEVal -> Val
feValToAll = FEVal

typeOf :: Val -> NonEmpty Ty
typeOf (BoolVal x) =
  fmap boolTyToAll (Booleans.typeOf x)
typeOf (FEVal x) =
  fmap feTyToAll (FieldElements.typeOf x)
typeOf (Curried _ _) =
  Ty :| [Ty]
typeOf Eq =
  BoolTy Booleans.Ty :| [FETy FieldElements.Ty, FETy FieldElements.Ty]
typeOf (IntegerVal _) = undefined

hasType :: Val -> PrimType Ty -> Bool
hasType x ty = ty == typeOf x

arity :: Val -> Int
arity = pred . length . typeOf


apply :: Val -> Val -> Maybe Val
apply (BoolVal x) (BoolVal y) =
  boolValToAll <$> Booleans.apply x y
apply (FEVal x) (FEVal y) =
  feValToAll <$> FieldElements.apply x y
apply Eq (FEVal f) = pure (Curried Eq f)
apply (Curried Eq (FieldElements.Val x)) (FEVal (FieldElements.Val y)) =
  pure (BoolVal (Booleans.Val (FieldElements.eq x y)))
apply _ _ = Nothing

parseTy :: Token.GenTokenParser String () Identity -> Parser Ty
parseTy lexer =
  (boolTyToAll <$> Booleans.parseTy lexer)
    <|> (feTyToAll <$> FieldElements.parseTy lexer)

parseVal :: Token.GenTokenParser String () Identity -> Parser Val
parseVal lexer =
  (boolValToAll <$> Booleans.parseVal lexer)
    <|> (feValToAll <$> FieldElements.parseVal lexer)

reservedNames :: [String]
reservedNames =
  Booleans.reservedNames <> FieldElements.reservedNames <> ["=="]

reservedOpNames :: [String]
reservedOpNames =
  Booleans.reservedOpNames <> FieldElements.reservedOpNames <> ["=="]

t :: Parameterisation Ty Val
t =
  Parameterisation {
    hasType, arity, apply, parseTy, parseVal, reservedNames, reservedOpNames,
    stringTy = \_ _ -> False,
    stringVal = const Nothing,
    intTy = \i _ -> False, -- TODO
    intVal = const Nothing, -- TODO
    floatTy = \_ _ -> False,
    floatVal = const Nothing
  }
