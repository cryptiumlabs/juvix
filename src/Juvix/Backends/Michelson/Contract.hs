-- |
-- This module provides a default contract environment
module Juvix.Backends.Michelson.Contract where

import qualified Michelson.Interpret as Interpt
import qualified Michelson.Test as Test

-- TODO ∷ make a real environment
-- Though does it matter?
contract :: Interpt.ContractEnv
contract = Test.dummyContractEnv
