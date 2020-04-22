module Juvix.Backends.ArithmeticCircuit.Compilation (compile, eval) where

import Juvix.Library hiding (Type)
import Juvix.Backends.ArithmeticCircuit.Compilation.Types
import qualified Circuit as Circuit
import qualified Circuit.Expr as Expr
import qualified Juvix.Backends.ArithmeticCircuit.Parameterisation.Booleans as Booleans
import qualified Juvix.Backends.ArithmeticCircuit.Parameterisation.FieldElements as FieldElements
import qualified Juvix.Backends.ArithmeticCircuit.Parameterisation as Parameterisation

compile :: Term -> Type -> Either () (Circuit.ArithCircuit FieldElements.F)
compile term _ = Right $ -- #FIXME bogus handling for now
  Expr.execCircuitBuilder (Expr.compile (f term))
  where f = undefined
