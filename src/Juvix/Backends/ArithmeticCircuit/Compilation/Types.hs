module Juvix.Backends.ArithmeticCircuit.Compilation.Types where

import qualified Circuit
import qualified Circuit.Expr as Expr
import qualified Juvix.Backends.ArithmeticCircuit.Parameterisation as Par
import qualified Juvix.Core.ErasedAnn as CoreErased
import Juvix.Library
import Numeric.Natural ()

data PrimVal
  = Element Par.F
  | Boolean Bool
  | FEInteger Int
  | BinOp BinOp Term Term
  | UnaryOp UnaryOp Term
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

data UnaryOp = Neg
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
