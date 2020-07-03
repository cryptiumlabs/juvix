{-# LANGUAGE NamedFieldPuns #-}

module Juvix.Backends.ArithmeticCircuit.ZKP where

import qualified Circuit
import qualified Data.Field.Galois as Galois
import qualified Data.Map as Map
import qualified Data.Pairing.BN254 as BN254
import qualified Fresh
import qualified Juvix.Backends.ArithmeticCircuit.Compilation as Base
import Juvix.Library
import qualified Protocol.Groth as Groth
import qualified QAP

data SetupOutput
  = Setup
      { randsetup :: Groth.RandomSetup BN254.Fr,
        qap :: QAP.QAP BN254.Fr,
        ref :: Groth.Reference (BN254.G1 BN254.BN254) (BN254.G2 BN254.BN254),
        roots :: [[BN254.Fr]]
      }

runSetup :: Circuit.ArithCircuit BN254.Fr -> IO SetupOutput
runSetup program = do
  let roots =
        Circuit.generateRoots (fmap (fromIntegral . succ) Fresh.fresh) program
          |> Fresh.evalFresh
  let qap = QAP.arithCircuitToQAP roots program
  --
  rndSetup <- Groth.generateRandomSetup Galois.rnd
  --
  let (ref, _) = Groth.setup rndSetup qap
  return $
    Setup
      { randsetup = rndSetup,
        qap = qap,
        ref = ref,
        roots = roots
      }

runProve ::
  Circuit.ArithCircuit BN254.Fr ->
  [(Int, BN254.Fr)] ->
  SetupOutput ->
  IO (Groth.Proof (BN254.G1 BN254.BN254) (BN254.G2 BN254.BN254))
runProve program inputs Setup {qap, ref} = do
  rndProver <- Groth.generateRandomProver Galois.rnd
  pure $ Groth.prove rndProver program qap (Groth.refP ref) (Map.fromList inputs)

runVerify ::
  [(Int, BN254.Fr)] ->
  SetupOutput ->
  Groth.Proof (BN254.G1 BN254.BN254) (BN254.G2 BN254.BN254) ->
  Bool
runVerify inputs Setup {ref} proof =
  Groth.verify (Groth.refV ref) (Map.fromList inputs) proof

verify ::
  [(Int, BN254.Fr)] ->
  SetupOutput ->
  Groth.Proof (BN254.G1 BN254.BN254) (BN254.G2 BN254.BN254) ->
  Bool
verify = runVerify

prove ::
  Base.Term ->
  Base.Type ->
  [(Int, BN254.Fr)] ->
  SetupOutput ->
  IO (Groth.Proof (BN254.G1 BN254.BN254) (BN254.G2 BN254.BN254))
prove term ty params stp =
  case Base.compile term ty of
    (Right _, program) -> runProve program params stp
    (Left error, _) -> panic $ show error
