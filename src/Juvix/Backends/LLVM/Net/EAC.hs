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

-- this function work off the nodeType signature not Types.eac

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

-- TODO ∷ make fast fanInAux and slow fanInAux

-- | 'fanInAuxStar' is a slower version of 'fanInAux*' where * ∈ ℤ/4ℤ.
-- This function is used when it can not be determined that 'fanInAux*'
fanInAuxStar = undefined

fanInAux0 allocF = Codegen.defineFunction Type.void "fan_in_aux_0" args $
  do
    era1 ← allocF
    era2 ← allocF
    undefined
  where
    args =
      [ (Codegen.nodeType, "node"), -- we send in the alloc function for it. Do case before
        (Codegen.nodeType, "fanIn") -- we know this must be a fanIn so no need for tag
      ]

fanInAux1 = undefined

fanInAux2 = undefined

fanInAux3 = undefined


--------------------------------------------------------------------------------
-- Allocations
--------------------------------------------------------------------------------
allocaEra = do
  era ← Codegen.alloca Types.eac
  node ← Codegen.allocaNodeH [Nothing] []
  tag ← Codegen.getElementPtr $
    Codegen.Minimal
    { Codegen.type' = Types.tag,
      Codegen.address' = era,
      Codegen.indincies' = Codegen.constant32List [0,1]
    }
  Codegen.store tag (Operand.ConstantOperand Types.era)
  nodePtr ← Codegen.getElementPtr $
    Codegen.Minimal
    { Codegen.type' = Types.tag,
      Codegen.address' = era,
      Codegen.indincies' = Codegen.constant32List [0,1]
    }
  Codegen.store nodePtr node
