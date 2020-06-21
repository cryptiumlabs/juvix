module Juvix.Backends.ArithmeticCircuit.Compilation.Types where

import qualified Juvix.Core.ErasedAnn as CoreErased
import qualified Juvix.Backends.ArithmeticCircuit.Parameterisation as Par
import qualified Circuit.Expr as Expr
import qualified Circuit
import Numeric.Natural()
import Juvix.Library

data PrimVal
  = Element Par.F
  | Boolean Bool
  | FEInteger Int
  | BinOp BinOp Term Term
  | Op Op Term
  | If Term Term Term
  deriving (Show, Eq)

data BinOp
  = Add
  | Mul
  | Sub
  | Exp
  | Eq
  | And
  | Or
  deriving (Show, Eq)

data Op = Neg
  deriving (Show, Eq)

type Term = CoreErased.AnnTerm () PrimVal
type Type = CoreErased.Type () PrimVal

data CompilationError
  = NotYetImplemented
  | SomethingWentWrongSorry
  | VariableOutOfScope
  | PrimTypeError
  | TypeErrorApplicationNonFunction
  deriving (Eq, Show, Generic)

data ArithExpression
  = BoolExp (Expr.Expr Circuit.Wire Par.F Bool)
  | FExp (Expr.Expr Circuit.Wire Par.F Par.F)
  | NoExp
  deriving (Generic)
