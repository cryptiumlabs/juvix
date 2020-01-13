-- |
-- Serves as a module for various debugging functions
module Juvix.Backends.LLVM.Codegen.Graph.Debug where

import qualified Juvix.Backends.LLVM.Codegen.Block as Block
import qualified Juvix.Backends.LLVM.Codegen.Graph.Operations as Ops
import qualified Juvix.Backends.LLVM.Codegen.Types as Types
import Juvix.Library hiding (reduce)
import qualified LLVM.AST.Operand as Operand

printNodePort ∷
  ( Types.Call m,
    Types.NewBlock m
  ) ⇒
  Operand.Operand →
  Operand.Operand →
  m ()
printNodePort nodePtr port = do
  portEra ← Ops.getPort nodePtr port
  pointToNode ← Block.loadElementPtr $
    Types.Minimal
      { Types.type' = Types.nodePointer,
        Types.address' = portEra,
        Types.indincies' = Block.constant32List [0, 0]
      }
  -- TODO ∷ use intOfNumPorts instead!
  port ← Block.loadElementPtr $
    Types.Minimal
      { Types.type' = Types.numPortsNameRef,
        Types.address' = portEra,
        Types.indincies' = Block.constant32List [0, 1]
      }
  -- TODO ∷ use intOfNumPorts!
  _ ←
    Block.printCString
      "node: %p at port: %i : < port: %p, node: %p > \n"
      [nodePtr, port, pointToNode, port]
  pure ()
