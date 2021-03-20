module Test.Groth where

import Data.Curve.Weierstrass.BLS12381 (BLS12381, Fr)
import qualified Data.Map as Map
import Data.Pairing (Pairing (..))
import Juvix.Backends.Plonk (FF (..), FFAnnTerm, FFType, PrimVal (..))
import qualified Juvix.Backends.Plonk as P
import qualified Juvix.Core.ErasedAnn as Core
import Juvix.Library hiding (exp)
import qualified Juvix.Library.Usage as Usage
import Ref.Groth
import Ref.Groth.Fresh
import Ref.Groth.QAP
import Test.AnnTerm
import qualified Test.QuickCheck.Monadic as QCM
import Test.Ref.Polynomial (circuitPolynomial1)
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T
import qualified Test.Tasty.QuickCheck as T

-------------------------------------------------------------------------------
-- Tests
-------------------------------------------------------------------------------

top :: T.TestTree
top =
  T.testGroup
    "Proof Systems tests"
    [groth]

groth :: T.TestTree
groth =
  T.testGroup
    "Polynomials"
    [T.testProperty "\\x y -> x^3 - 2x^2 + 4 = y" polynomial1]

polynomial1 ::
  RandomSetup Fr ->
  RandomProver Fr ->
  T.Property
polynomial1 rndSetup rndProver =
  T.withMaxSuccess 3 $
    let inputs = Map.fromList [(P.InputWire 0, 1), (P.InputWire 1, 3)]
     in testVerification inputs
  where
    roots = evalFresh $ P.generateRoots (fromIntegral <$> fresh) circuitPolynomial1
    qap = arithCircuitToQAP roots circuitPolynomial1
    testReference = fst $ setup rndSetup qap
    testProof :: Map P.Wire Fr -> Proof (G1 BLS12381) (G2 BLS12381)
    testProof = prove rndProver circuitPolynomial1 qap (refP testReference)
    testVerification :: Map P.Wire Fr -> Bool
    testVerification input = verify (refV testReference) input (testProof input)
