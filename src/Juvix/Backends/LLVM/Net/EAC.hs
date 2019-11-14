module Juvix.Backends.LLVM.Net.EAC where

-- TODO ∷ abstract all all imports to LLVM

import qualified Juvix.Backends.LLVM.Codegen as Codegen
import qualified Juvix.Backends.LLVM.Net.EAC.Types as Types
import qualified LLVM.AST.Type as Type
import Juvix.Library

--------------------------------------------------------------------------------
-- Reduction rules
--------------------------------------------------------------------------------

-- TODO ∷ Maybe add metadata at some point?

-- These functions work off the nodeType signature not Types.eac

-- mimic rules from the interpreter
anihilateRewireAux = Codegen.defineFunction Type.void "anihilate_rewire_aux" args $
  do
    undefined
  where
    args = [(Codegen.nodeType, "node_1"), (Codegen.nodeType, "node_2")]
