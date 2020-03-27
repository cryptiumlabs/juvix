module Juvix.Backends.ArithmeticCircuit.Compilation where

import qualified Bulletproofs.ArithmeticCircuit as C
import Data.Curve.Weierstrass.SECP256K1 (Fr)
import Juvix.Backends.ArithmeticCircuit.Compilation.Types
import Juvix.Backends.ArithmeticCircuit.Parameterisation
import Juvix.Library hiding (Type)

compile ::
  forall m.
  (Monad m) =>
  Term ->
  Type ->
  m (C.ArithCircuit Fr)
compile _ _ = do
  pure (flattenCircuit undefined)

flattenCircuit :: Circuit -> C.ArithCircuit Fr
flattenCircuit _ =
  C.ArithCircuit
    { C.weights = C.GateWeights {C.wL = [], C.wR = [], C.wO = []},
      C.commitmentWeights = [],
      C.cs = []
    }
{-
 - find all output values
 - follow wire back to gate
 - if add/mul: add add/mul constraint of input values & output or intermediate value
 - if input: stop
 - otherwise: continue
 -}
