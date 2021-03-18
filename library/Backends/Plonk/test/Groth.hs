module Groth where

import Data.Curve.Weierstrass.BLS12381 (BLS12381, Fr)
import qualified Data.Map as Map
import Data.Pairing (Pairing (..))
import FFExample
import Juvix.Backends.Plonk.Circuit
import Juvix.Library
import Ref.Groth
import Ref.Groth.Fresh
import Ref.Groth.QAP
import qualified Test.QuickCheck.Monadic as QCM
import Test.Tasty.QuickCheck

-------------------------------------------------------------------------------
-- Tests
-------------------------------------------------------------------------------

-- Example 1: Case with intermediate wire
-- a - input wire
-- i - intermediate wire
-- o - output wire
--          o0 |
--            [X]
--         i0 / \
--          [X]  a2
--          / \
--        a0   a1
testArithCircuit1 :: ArithCircuit Fr
testArithCircuit1 = plonkExample

-- ArithCircuit
--   [ MulGate (Var (InputWire 0)) (Var (InputWire 1)) (IntermediateWire 0),
--     MulGate (Add (ConstGate 1) (Var (IntermediateWire 0))) (ScalarMul 7 (Var (InputWire 2))) (OutputWire 0)
--   ]

prop_example_1_rnd_inputs ::
  RandomSetup Fr ->
  RandomProver Fr ->
  Property
prop_example_1_rnd_inputs rndSetup rndProver =
  withMaxSuccess 5 $ QCM.monadicIO $ do
    -- inputs <- lift . generate $ arbInputVector 2
    let inputs = Map.fromList [(1, 5), (2, 6), (3, 7), (4, 8)]
    lift . pure $ testVerification inputs
  where
    roots = evalFresh $ generateRoots (fromIntegral <$> fresh) testArithCircuit1
    qap = arithCircuitToQAP roots testArithCircuit1
    testReference = fst $ setup rndSetup qap
    testProof :: Map Int Fr -> Proof (G1 BLS12381) (G2 BLS12381)
    testProof = prove rndProver testArithCircuit1 qap (refP testReference)
    testVerification :: Map Int Fr -> Bool
    testVerification input = verify (refV testReference) input (testProof input)

arbInputVector :: (Arbitrary f) => Int -> Gen (Map Int f)
arbInputVector numVars = Map.fromList . zip [0 ..] <$> vector numVars
