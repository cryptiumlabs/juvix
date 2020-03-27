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

-- all primitive types
data Ty
  = FETy FieldElements.Ty
  | BoolTy Booleans.Ty
  deriving (Show, Eq)

type BooleanVal = Booleans.Val FieldElements.F

-- c: primitive constant and f: functions
data Val
  = FEVal FieldElements.Val
  | BoolVal BooleanVal
  deriving (Show, Eq)

boolTyToAll ∷ Booleans.Ty → Ty
boolTyToAll = BoolTy

boolValToAll ∷ BooleanVal → Val
boolValToAll = BoolVal

feTyToAll ∷ FieldElements.Ty → Ty
feTyToAll = FETy

feValToAll ∷ FieldElements.Val → Val
feValToAll = FEVal

typeOf ∷ Val → NonEmpty Ty
typeOf (BoolVal x) =
  fmap boolTyToAll (Booleans.typeOf x)
typeOf (FEVal x) =
  fmap feTyToAll (FieldElements.typeOf x)

apply ∷ Val → Val → Maybe Val
apply (BoolVal x) (BoolVal y) =
  boolValToAll <$> Booleans.apply x y
apply (FEVal x) (FEVal y) =
  feValToAll <$> FieldElements.apply x y
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
reservedNames = Booleans.reservedNames <> FieldElements.reservedNames

reservedOpNames ∷ [String]
reservedOpNames = Booleans.reservedOpNames <> FieldElements.reservedOpNames

t ∷ Parameterisation Ty Val
t =
  Parameterisation typeOf apply parseTy parseVal reservedNames reservedOpNames
