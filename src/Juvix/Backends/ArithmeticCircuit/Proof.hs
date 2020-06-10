module Proof (proof) where

import Circuit.Arithmetic (ArithCircuit)
import Data.Aeson
import qualified Data.Curve.Weierstrass.BN254 as G1
import qualified Data.Curve.Weierstrass.BN254T as G2
import Data.Field.Galois (Extension, rnd, toP)
import Data.Pairing.BN254 (BN254, Fr, Pairing (..))
import qualified Protocol.Groth as Groth
import Protolude hiding (readFile)
import QAP (QAP)
import System.Directory
import System.FilePath.Posix
import Prelude (read, readFile)

deriving instance FromJSON G1.PA

deriving instance FromJSON G2.PA

deriving instance ToJSON G1.PA

deriving instance ToJSON G2.PA

deriving instance FromJSON (Extension G2.U G1.Fq)

deriving instance ToJSON (Extension G2.U G1.Fq)

runProof :: FilePath -> FilePath -> FilePath -> FilePath -> IO ()
runProof programInput setupDir inputFile piFile = do
  -- Read the program inputs as JSON`
  Just (program :: ArithCircuit Fr) <- decodeFileStrict programInput
  -- Read the random setup
  Just (setup :: Groth.RandomSetup Fr) <- decodeFileStrict (setupDir </> "setup")
  -- Read the quadratic arithmetic program
  Just (qap :: QAP Fr) <- decodeFileStrict (setupDir </> "qap")
  -- Read the common reference string
  Just (reference :: Groth.Reference (G1 BN254) (G2 BN254)) <- decodeFileStrict (setupDir </> "ref")
  -- Read the program inputs
  Just (inputs :: Map Int Fr) <- decodeFileStrict inputFile
  --qap <- read <$> readFile setupDir
  rndProver <- Groth.generateRandomProver rnd
  let proof = Groth.prove rndProver program qap (Groth.refP reference) inputs
  encodeFile piFile proof
