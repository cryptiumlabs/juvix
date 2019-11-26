module Juvix.Backends.LLVM.Net.EAC.Types where

import qualified Juvix.Backends.LLVM.Codegen as Codegen
import Juvix.Library
-- Abstract out LLVM imports?

import qualified LLVM.AST.AddrSpace as Addr
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Type as Type

-- defines a tag over a node type, to denote the variant

tag ∷ Type.Type
tag = Codegen.i4

tagInt ∷ Num p ⇒ p
tagInt = 4

eac ∷ Type.Type
eac = Type.StructureType
  { Type.isPacked = True,
    Type.elementTypes =
      [ tag,
        Codegen.nodeType
      ]
  }

app, dup, lam, era ∷ C.Constant
app = C.Int {C.integerBits = tagInt, C.integerValue = 0}
lam = C.Int {C.integerBits = tagInt, C.integerValue = 1}
era = C.Int {C.integerBits = tagInt, C.integerValue = 2}
dup = C.Int {C.integerBits = tagInt, C.integerValue = 3}

eacList ∷ Type.Type
eacList = Type.StructureType
  { -- change to true later?
    Type.isPacked = False,
    Type.elementTypes = [eac, eacLPointer]
  }

eacLPointer ∷ Type.Type
eacLPointer = Type.PointerType eacList (Addr.AddrSpace 32)
