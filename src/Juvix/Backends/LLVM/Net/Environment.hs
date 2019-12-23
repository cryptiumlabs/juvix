-- |
-- - Serves as the default environment for executing EAC code
module Juvix.Backends.LLVM.Net.Environment where

import qualified Juvix.Backends.LLVM.Codegen as Codegen
import qualified Juvix.Backends.LLVM.Net.API as API
import qualified Juvix.Backends.LLVM.Net.EAC as EAC
import qualified Juvix.Backends.LLVM.Net.EAC.Defs as Defs
import qualified Juvix.Backends.LLVM.Net.EAC.Types as Types
import Juvix.Library
import qualified Juvix.Library.HashMap as Map

-- import qualified Juvix.Backends.LLVM.JIT as JIT

initialModule ∷
  ( Codegen.Define m,
    HasState "typTab" Codegen.TypeTable m,
    HasState "varTab" Codegen.VariantToType m
  ) ⇒
  m ()
initialModule = do
  modify @"typTab"
    ( Map.insert "numPorts" Codegen.numPorts
        . Map.insert "numPorts_large" (Codegen.numPortsLargeType)
        . Map.insert "numPorts_small" (Codegen.numPortsSmallType)
    )
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
  -- registering types----------------------------------------------
  Codegen.addType "list" Types.testList
  Codegen.addType Codegen.numPortsName Codegen.numPorts
  Codegen.addType Codegen.portTypeName Defs.portType
  Codegen.addType Types.eacName Types.eac
  Codegen.addType Types.eacListName Types.eacList
  -- ---------------------------------------------------------------
  Codegen.addBlock "bad" >>= Codegen.setBlock
  Codegen.defineMalloc
  Codegen.defineFree
  Codegen.defineMainPort
  Codegen.defineAuxiliary1
  Codegen.defineAuxiliary2
  Codegen.defineAuxiliary3
  Codegen.defineAuxiliary4
  _ ← Codegen.defineLink
  _ ← EAC.defineTest
  -- _ ← Codegen.alloca Types.testListPointer
  _ ← Codegen.defineFindEdge
  _ ← Defs.defineIsBothPrimary
  _ ← Defs.defineLinkConnectedPort
  _ ← Defs.defineRewire
  _ ← EAC.defineEraseNodes
  _ ← EAC.defineFanInAux0E
  _ ← EAC.defineFanInAux2A
  _ ← EAC.defineFanInAux2L
  _ ← EAC.defineFanInAux2F

  _ ← EAC.defineAnnihilateRewireAux
  _ ← EAC.defineFanInFanIn
  -- _ <- EAC.defineReduce

  -- define the API
  --Codegen.addType "node" API.node
  --_ ← API.defineCreateNet
  --_ ← API.defineReadNet
  --_ ← API.defineAppendToNet
  --_ ← API.defineReduceUntilComplete
  pure ()

runInitModule ∷ Codegen.CodegenState
runInitModule = Codegen.execEnvState initialModule Map.empty

runInitModule' ∷ Either Codegen.Errors ()
runInitModule' = Codegen.evalEnvState initialModule Map.empty
