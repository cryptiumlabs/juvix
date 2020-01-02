-- |
-- Serves as a module for various debugging functions
module Juvix.Backends.LLVM.Net.EAC.Debug where

import qualified Juvix.Backends.LLVM.Codegen as Codegen
import Juvix.Library hiding (reduce)
import qualified Juvix.Library.HashMap as Map
import qualified LLVM.AST.Name as Name
import qualified LLVM.AST.Operand as Operand
import qualified LLVM.AST.Type as Type

printNodePort ∷
  ( HasThrow "err" Codegen.Errors m,
    HasState "blockCount" Int m,
    HasState "blocks" (Map.T Name.Name Codegen.BlockState) m,
    HasState "count" Word m,
    HasState "currentBlock" Name.Name m,
    HasState "names" Codegen.Names m,
    HasState "symTab" Codegen.SymbolTable m
  ) ⇒
  Operand.Operand →
  Operand.Operand →
  m ()
printNodePort nodePtr port = do
  portEra ← Codegen.getPort nodePtr port
  pointToNode ← Codegen.loadElementPtr $
    Codegen.Minimal
      { Codegen.type' = Codegen.nodePointer,
        Codegen.address' = portEra,
        Codegen.indincies' = Codegen.constant32List [0, 0]
      }
  -- TODO ∷ use intOfNumPorts instead!
  port ← Codegen.loadElementPtr $
    Codegen.Minimal
      { Codegen.type' = Codegen.numPortsNameRef,
        Codegen.address' = portEra,
        Codegen.indincies' = Codegen.constant32List [0, 1]
      }
  -- TODO ∷ use intOfNumPorts instead!
  p ← Codegen.load (Type.IntegerType Codegen.addressSpace) port
  _ ← Codegen.printCString "node: %p at port: %i : < port: %p, node: %p > \n" [nodePtr, p, pointToNode, port]
  pure ()
