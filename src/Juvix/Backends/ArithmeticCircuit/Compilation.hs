module Juvix.Backends.ArithmeticCircuit.Compilation (compile, runCirc, add, mul, sub, neg, eq, exp, int, c, and', or', Term, Type, lambda, var, input, cond) where

import Juvix.Library hiding (Type, exp)
import Juvix.Backends.ArithmeticCircuit.Compilation.Types
import qualified Circuit
import qualified Circuit.Expr as Expr
import qualified Juvix.Backends.ArithmeticCircuit.Parameterisation as Par

compile :: Term -> Type -> Circuit.ArithCircuit Par.F
compile term _ =
  let (_, circ) = transTerm mempty term
  in case circ of
    BoolExp exp -> Expr.execCircuitBuilder (Expr.compile exp)
    FExp exp -> Expr.execCircuitBuilder (Expr.compile exp)

runCirc = Expr.execCircuitBuilder . Expr.compile
