module Juvix.Backends.Michelson.DSL.Interpret where

import qualified Juvix.Backends.Michelson.DSL.Contract as Contract
import Juvix.Library
import qualified Michelson.Interpret as Interpret
import qualified Michelson.Untyped.Aliases as Alias
import qualified Michelson.Untyped.Value as Value

dummyInterpretContract ::
  Alias.Contract -> Either Interpret.InterpretError Interpret.InterpretResult
dummyInterpretContract contract =
  Interpret.interpretUntyped
    contract
    Value.ValueUnit
    Value.ValueUnit
    Contract.dummyContractEnv
