module Juvix.Backends.LLVM.Net.EAC where

import qualified Juvix.Backends.LLVM.Codegen as Codegen
import qualified Juvix.Backends.LLVM.Net.EAC.Types as Types
import Juvix.Library

--------------------------------------------------------------------------------
-- Reduction rules
--------------------------------------------------------------------------------

-- TODO ∷ Maybe add metadata at some point?

-- These functions work off the nodeType signature not Types.eac

-- mimic rules from the interpreter
anihilateRewireAux = undefined
  where
    args = [(Codegen.nodeType, "node_1"), (Codegen.nodeType, "node_2")]
    body = do
      undefined
