module Juvix.Backends.ArithmeticCircuit.Compilation.Types where

import qualified Juvix.Core.ErasedAnn.Types as CoreErased
import Juvix.Library
import qualified Juvix.Backends.ArithmeticCircuit.Parameterisation.FieldElements as FieldElements
import qualified Juvix.Backends.ArithmeticCircuit.Parameterisation as Parameterisation

data Val
  = FEVal FieldElements.Val
  | BoolVal Parameterisation.BooleanVal
  | Eq Term Term
  deriving Show


type Term = CoreErased.AnnTerm Parameterisation.Ty Val
type Type = CoreErased.Type Parameterisation.Ty Val
