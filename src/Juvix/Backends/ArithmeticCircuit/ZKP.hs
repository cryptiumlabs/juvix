{-# LANGUAGE NamedFieldPuns #-}

module Juvix.Backends.ArithmeticCircuit.ZKP where

import Circuit
import Control.Monad.Random ()
import qualified Data.Curve.Weierstrass.BN254 as G1
import qualified Data.Curve.Weierstrass.BN254T as G2
import Data.Field.Galois (rnd)
import qualified Data.Map as Map
import Data.Pairing.BN254 (BN254, Fr, Pairing (..))
import Fresh (evalFresh, fresh)
import Juvix.Backends.ArithmeticCircuit.Compilation as Base
import qualified Protocol.Groth as Groth
import Protolude
import QAP

data SetupOutput
  = Setup
      { randsetup :: Groth.RandomSetup Fr,
        qap :: QAP Fr,
        ref :: Groth.Reference (G1 BN254) (G2 BN254),
        roots :: [[Fr]]
      }

runSetup :: ArithCircuit Fr -> IO SetupOutput
runSetup program = do
  let roots = evalFresh (generateRoots (fmap (fromIntegral . (+ 1)) fresh) program)
  let qap = QAP.arithCircuitToQAP roots program
  rndSetup <- Groth.generateRandomSetup rnd
  let (ref, _) = Groth.setup rndSetup qap
  return $
    Setup
      { randsetup = rndSetup,
        qap = qap,
        ref = ref,
        roots = roots
      }

runProve :: ArithCircuit Fr -> [(Int, Fr)] -> SetupOutput -> IO (Groth.Proof (G1 BN254) (G2 BN254))
runProve program inputs Setup {qap, ref} = do
  rndProver <- Groth.generateRandomProver rnd
  return $ Groth.prove rndProver program qap (Groth.refP ref) (Map.fromList inputs)

runVerify :: [(Int, Fr)] -> SetupOutput -> Groth.Proof (G1 BN254) (G2 BN254) -> Bool
runVerify inputs Setup {ref} proof = Groth.verify (Groth.refV ref) (Map.fromList inputs) proof

verify :: [(Int, Fr)] -> SetupOutput -> Groth.Proof (G1 BN254) (G2 BN254) -> Bool
verify = runVerify

prove :: Base.Term -> Base.Type -> [(Int, Fr)] -> SetupOutput -> IO (Groth.Proof (G1 BN254) (G2 BN254))
prove term ty params stp =
  case Base.compile term ty of
    (Right _, program) -> runProve program params stp
    (Left error, _) -> panic $ show error
