module Juvix.Backends.ArithmeticCircuit.Parameterisation where

import qualified Juvix.Backends.ArithmeticCircuit.Parameterisation.Booleans as Booleans
import qualified Juvix.Backends.ArithmeticCircuit.Parameterisation.FieldElements as FieldElements
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
import qualified Circuit.Arithmetic as Arith
import qualified Circuit.Expr as Expr
import qualified Circuit.Lang as Lang

-- all primitive types
data Ty
  = FETy FieldElements.Ty
  | BoolTy Booleans.Ty
  | Ty
  deriving (Show, Eq)

type F = Fr
type BooleanVal = Booleans.Val (Expr.Expr Arith.Wire F Bool) Bool
type FEVal = FieldElements.Val (Expr.Expr Arith.Wire F F)

instance FieldElements.FieldElement (Expr.Expr Arith.Wire) where
  add = Lang.add
  mul = Lang.mul
  sub = Lang.sub
  neg = Expr.EUnOp Expr.UNeg
  eq  = Lang.eq

instance Booleans.Boolean (Expr.Expr Arith.Wire) Bool where
  and' = Lang.and_
  or' = Lang.or_
  not' = Lang.not_
  true = Expr.EConstBool True
  false = Expr.EConstBool False

data Val where
  FEVal :: FEVal -> Val
  BoolVal :: BooleanVal -> Val
  Eq :: Val
  Curried :: Val -> FEVal -> Val

boolTyToAll ∷ Booleans.Ty → Ty
boolTyToAll = BoolTy

boolValToAll ∷ BooleanVal → Val
boolValToAll = BoolVal

feTyToAll ∷ FieldElements.Ty → Ty
feTyToAll = FETy

feValToAll ∷ FEVal → Val
feValToAll = FEVal

typeOf ∷ Val → NonEmpty Ty
typeOf (BoolVal x) =
  fmap boolTyToAll (Booleans.typeOf x)
typeOf (FEVal x) =
  fmap feTyToAll (FieldElements.typeOf x)
typeOf (Curried _ _) = Ty :| [Ty]
typeOf Eq = BoolTy Booleans.Ty :| [FETy FieldElements.Ty, FETy FieldElements.Ty]


apply ∷ Val → Val → Maybe Val
apply (BoolVal x) (BoolVal y) =
  boolValToAll <$> Booleans.apply x y
apply (FEVal x) (FEVal y) =
  feValToAll <$> FieldElements.apply x y
apply Eq (FEVal f) = pure (Curried Eq f)
apply (Curried Eq (FieldElements.Val x)) (FEVal (FieldElements.Val y)) =
  pure (BoolVal (Booleans.Val (FieldElements.eq x y)))
apply _ _ = Nothing

parseTy ∷ Token.GenTokenParser String () Identity → Parser Ty
parseTy lexer =
  (boolTyToAll <$> Booleans.parseTy lexer) <|>
  (feTyToAll <$> FieldElements.parseTy lexer)

parseVal ∷ Token.GenTokenParser String () Identity → Parser Val
parseVal lexer =
  (boolValToAll <$> Booleans.parseVal lexer) <|>
  (feValToAll <$> FieldElements.parseVal lexer)

reservedNames ∷ [String]
reservedNames = Booleans.reservedNames <> FieldElements.reservedNames <> ["=="]

reservedOpNames ∷ [String]
reservedOpNames = Booleans.reservedOpNames <> FieldElements.reservedOpNames <> ["=="]

t ∷ Parameterisation Ty Val
t =
  Parameterisation typeOf apply parseTy parseVal reservedNames reservedOpNames
