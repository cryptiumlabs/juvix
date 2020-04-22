module Backends.ArithmeticCircuit where

import Juvix.Backends.ArithmeticCircuit.Compilation
import Juvix.Backends.ArithmeticCircuit.Compilation.Types
import Juvix.Backends.ArithmeticCircuit.Parameterisation
import qualified Juvix.Backends.ArithmeticCircuit.Parameterisation.FieldElements as FE
import qualified Juvix.Core.Erased.Types as J
import Juvix.Core.Usage
import Juvix.Library hiding (Type)
import qualified Circuit as Circuit
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

shouldCompile :: Term -> Type -> Circuit.ArithCircuit f -> T.TestTree
shouldCompile term ty circuit =
  T.testCase
    (show term <> " :: " <> show ty <> " should compile to " <> show circuit)
    (compile term ty T.@=? Just circuit)

-- backendCircuit :: T.TestTree
-- backendCircuit =
--   T.testGroup
--     "Backend arithmetic circuit"
--     [shouldCompile equalTerm equalType equalCircuit]

-- equalTerm :: Term
-- equalTerm = J.Lam "x" (J.Var "x")

-- equalType :: Type
-- equalType = J.Pi Omega (J.PrimTy ()) (J.PrimTy ())

power :: Term
power = J.Lam undefined undefined
-- equalCircuit :: C.ArithCircuit Fr
-- equalCircuit =
--   C.ArithCircuit
--     { C.weights = C.GateWeights {C.wL = [], C.wR = [], C.wO = []},
--       C.commitmentWeights = [],
--       C.cs = []
--     }
