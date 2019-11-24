module Backends.ArithmeticCircuit where

import qualified Bulletproofs.ArithmeticCircuit as C
import Data.Curve.Weierstrass.SECP256K1 (Fr)
import Juvix.Backends.ArithmeticCircuit.Compilation
import Juvix.Backends.ArithmeticCircuit.Parameterisation
import qualified Juvix.Core.Erased.Types as J
import Juvix.Library hiding (Type)
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

shouldCompile ∷ Term → Type → C.ArithCircuit Fr → T.TestTree
shouldCompile term ty circuit =
  T.testCase
    (show term <> " :: " <> show ty <> " should compile to " <> show circuit)
    (compile term ty T.@=? Just circuit)

test_equal ∷ T.TestTree
test_equal = shouldCompile equalTerm equalType equalCircuit

equalTerm ∷ Term
equalTerm = J.Lam "x" (J.Var "x")

equalType ∷ Type
equalType = J.Pi (J.PrimTy ()) (J.PrimTy ())

equalCircuit ∷ C.ArithCircuit Fr
equalCircuit = C.ArithCircuit
  { C.weights = C.GateWeights {C.wL = [], C.wR = [], C.wO = []},
    C.commitmentWeights = [],
    C.cs = []
  }
