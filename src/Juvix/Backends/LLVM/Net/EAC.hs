module Juvix.Backends.LLVM.Net.EAC where

-- TODO ∷ abstract all all imports to LLVM

import qualified Juvix.Backends.LLVM.Codegen as Codegen
import qualified Juvix.Backends.LLVM.Net.EAC.Types as Types
import Juvix.Library
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Operand as Operand
import qualified LLVM.AST.Type as Type

--------------------------------------------------------------------------------
-- Interaction Net runner
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Reduction rules
--------------------------------------------------------------------------------

-- TODO ∷ Maybe add metadata at some point?

-- These functions work off the nodeType signature not Types.eac

-- mimic rules from the interpreter
-- This rule applies to Application ↔ Lambda
anihilateRewireAux = Codegen.defineFunction Type.void "anihilate_rewire_aux" args $
  do
    -- TODO remove these explicit allocations
    aux1 ← Codegen.allocaNumPortsStatic False (Operand.ConstantOperand (C.Int 32 1))
    aux2 ← Codegen.allocaNumPortsStatic False (Operand.ConstantOperand (C.Int 32 2))
    rewire ← Codegen.externf "rewire"
    node1 ← Codegen.externf "node_1"
    node2 ← Codegen.externf "node_2"
    _ ← Codegen.call Type.void rewire (Codegen.emptyArgs [node1, aux1, node2, aux1])
    _ ← Codegen.call Type.void rewire (Codegen.emptyArgs [node1, aux2, node2, aux2])
    _ ← Codegen.delNode node1
    Codegen.delNode node2
  where
    args = [(Codegen.nodeType, "node_1"), (Codegen.nodeType, "node_2")]


-- TODO ∷ make fast rewires and slow rewires
