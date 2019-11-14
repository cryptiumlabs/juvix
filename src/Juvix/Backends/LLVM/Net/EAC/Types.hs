module Juvix.Backends.LLVM.Net.EAC.Types where

import qualified Juvix.Backends.LLVM.Codegen as Codegen
import Juvix.Library
-- Abstract out LLVM imports?
import qualified LLVM.AST.Type as Type

-- defines a tag over a node type, to denote the variant

tag ∷ Type.Type
tag = Codegen.i4

eac ∷ Type.Type
eac = Type.StructureType
  { Type.isPacked = True,
    Type.elementTypes =
      [ tag,
        Codegen.nodeType
      ]
  }
