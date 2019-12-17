-- |
-- - Serves as the default environment for executing EAC code
module Juvix.Backends.LLVM.Net.Environment where

import qualified Juvix.Backends.LLVM.Codegen as Codegen
import qualified Juvix.Backends.LLVM.Net.EAC as EAC
import qualified Juvix.Backends.LLVM.Net.EAC.Defs as Defs
import qualified Juvix.Backends.LLVM.Net.EAC.Types as Types
import Juvix.Library
import qualified Juvix.Library.HashMap as Map

import qualified Juvix.Backends.LLVM.JIT as JIT

initialModule ∷
  ( Codegen.Define m,
    HasState "typTab" Codegen.TypeTable m,
    HasState "varTab" Codegen.VariantToType m
  ) ⇒
  m ()
initialModule = do
  modify @"typTab" (Map.insert "numPorts" Codegen.numPorts
                   . Map.insert "numPorts_large" (Codegen.numPortsLargeValuePtr)
                   . Map.insert "numPorts_small" (Codegen.numPortsSmallValue))
  modify @"varTab"
    ( Map.insert "numPorts_small" Codegen.S
        { Codegen.sum' = "numPorts",
          Codegen.offset = 0,
          Codegen.tagSize' = 1
        }
        . Map.insert "numPorts_large" Codegen.S
          { Codegen.sum' = "numPorts",
            Codegen.offset = 1,
            Codegen.tagSize' = 1
          }
    )
  Codegen.addBlock "bad" >>=  Codegen.setBlock
  Codegen.defineMalloc
  Codegen.defineFree
  Codegen.defineMainPort Types.eacPointer
  Codegen.defineAuxiliary1 Types.eacPointer
  Codegen.defineAuxiliary2 Types.eacPointer
  Codegen.defineAuxiliary3 Types.eacPointer
  Codegen.defineAuxiliary4 Types.eacPointer
  -- _ ← Defs.defineIsBothPrimary
  -- _ ← Defs.defineFindEdge
  -- _ ← Defs.defineLink
  -- _ ← Defs.defineRewire
  -- _ ← Defs.defineLinkConnectedPort
  -- _ ← EAC.defineFanInAux2F
  -- _ ← EAC.defineFanInAux2A
  -- _ ← EAC.defineFanInAux2L
  -- _ ← EAC.defineFanInAux2E
  -- _ ← EAC.defineAnnihilateRewireAux
  -- _ ← EAC.defineEraseNodes
  -- _ ← EAC.defineFanInFanIn
  pure ()


runInitModule ∷ Codegen.CodegenState
runInitModule = Codegen.execEnvState initialModule Map.empty


runInitModule' :: Either Codegen.Errors ()
runInitModule' = Codegen.evalEnvState initialModule Map.empty
