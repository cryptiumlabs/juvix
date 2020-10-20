-- |
-- - place holder module until we have our groth
--   setup done
module Juvix.Backends.ArithmeticCircuit.Groth where

import qualified Circuit
import qualified Data.Pairing.BN254 as BN254
import Juvix.Library
import qualified QAP

data RandomSetup a = Setup a

data Reference a b = Reference a b

data Proof a b = Proof a b

data RandomProver a = Prover a

data RefP a b = RefP a b

generateRandomSetup :: Monad m => m f -> m (RandomSetup f)
generateRandomSetup _ = undefined

setup _ _ = undefined

generateRandomProver ::
  Monad m => m f -> m (RandomProver f)
generateRandomProver _ = undefined

prove ::
  RandomProver BN254.Fr ->
  Circuit.ArithCircuit BN254.Fr ->
  QAP.QAP BN254.Fr ->
  RefP BN254.G1' BN254.G2' ->
  Map Int BN254.Fr ->
  Proof BN254.G1' BN254.G2'
prove _ _ _ _ _ = undefined

refP _ = undefined

refV _ = undefined

verify _ _ _ = undefined
