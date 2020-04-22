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
type Circuit f g = Expr.Expr Arith.Wire f g
type BooleanVal = Booleans.Val (Circuit F Bool)
type FEVal = FieldElements.Val (Circuit F F)


instance FieldElements.FieldElement (Circuit f f) where
  add = Lang.add
  mul = Lang.mul
  sub = Lang.sub
  neg = Expr.EUnOp Expr.UNeg
  -- eq  = Lang.eq
  -- skipping integer exponentiation as `arithmetic-circuits` doesn't implement it

data Val
  = FEVal FEVal
  | BoolVal BooleanVal
  | Eq
  | CurriedF Val ()

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
typeOf (CurriedF _ _) = Ty :| [Ty]
typeOf Eq = BoolTy Booleans.Ty :| [FETy FieldElements.Ty, FETy FieldElements.Ty]


apply ∷ Val → Val → Maybe Val
apply (BoolVal x) (BoolVal y) =
  boolValToAll <$> Booleans.apply x y
apply (FEVal x) (FEVal y) =
  feValToAll <$> FieldElements.apply x y
-- apply Eq (FEVal (FieldElements.Val x)) = pure (CurriedF Eq x)
-- apply (CurriedF Eq x) (FEVal (FieldElements.Val y)) = pure (BoolVal (Booleans.Val (eq x y)))
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
