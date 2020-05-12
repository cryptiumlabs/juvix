module Juvix.Backends.Michelson.DSL.Interpret where

import qualified Data.Vinyl.Core as Vinyl
import qualified Juvix.Backends.Michelson.Compilation.Types as Types
import qualified Juvix.Backends.Michelson.DSL.Contract as Contract
import Juvix.Library
import qualified Michelson.Interpret as Interpret
import qualified Michelson.Typed.Aliases as TAlias
import qualified Michelson.Typed.Instr as Instr
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

-- dummyInterpret ::
--   Types.EmptyInstr
--   -> Either Interpret.MichelsonFailed (Vinyl.Rec TAlias.Value out)
dummyInterpret (Types.EmptyInstr inst) =
  case Interpret.interpretInstr Contract.dummyContractEnv inst Vinyl.RNil of
    Left _ -> undefined
    Right (x Vinyl.:& _) -> undefined
    Right _ -> undefined
