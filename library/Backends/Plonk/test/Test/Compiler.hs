-- Finite fields example
module Test.Compiler where

import Data.Curve.Weierstrass.BLS12381 (Fr)
import Data.Field.Galois (GaloisField)
import qualified Data.Map as Map
import Juvix.Backends.Plonk (FFAnnTerm, FFType, PrimVal (..))
import qualified Juvix.Backends.Plonk as P
import Juvix.Library (($), (.), Natural, undefined)
import Juvix.Library hiding (Type, exp)
import Test.Ref.Polynomial (circuitPolynomial1)
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

top :: T.TestTree
top =
  T.testGroup
    "Compiler tests"
    [polynomials]

polynomials :: T.TestTree
polynomials =
  T.testGroup
    "Polynomials"
    [polynomial1]

testOutput :: (GaloisField f, Bits f) => P.ArithCircuit f -> Map P.Wire f -> f -> T.Assertion
testOutput circuit inputs expectedOutput = expectedOutput T.@=? actualOutput
  where
    assignment = P.generateAssignment circuit inputs
    actualOutput = maybe (panic $ "Not output found: " <> show assignment) snd (head $ Map.toList (P.assignmentOutput assignment)) -- There should be only one output

mkInputs :: [f] -> Map P.Wire f
mkInputs inputs = Map.fromList $ zipWith (\k v -> (P.InputWire k, v)) [0 ..] inputs

polynomial1 :: T.TestTree
polynomial1 = T.testCase "\\x y -> x^3 - 2x^2 + 4 = y" (testOutput circuitPolynomial1 inputs output)
  where
    inputs = mkInputs [1, 3]
    output = 1 -- true!
