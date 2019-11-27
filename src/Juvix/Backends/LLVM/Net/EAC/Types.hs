module Juvix.Backends.LLVM.Net.EAC.Types where

import qualified Juvix.Backends.LLVM.Codegen as Codegen
import Juvix.Library
import qualified Juvix.Library.HashMap as Map
-- Abstract out LLVM imports?

import qualified LLVM.AST.AddrSpace as Addr
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Name as Name
import qualified LLVM.AST.Operand as Operand
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
        Codegen.nodePointer
      ]
  }

eacPointer ∷ Type.Type
eacPointer = Type.PointerType eac (Addr.AddrSpace 32)

app, dup, lam, era ∷ C.Constant
app = C.Int {C.integerBits = tagInt, C.integerValue = 0}
lam = C.Int {C.integerBits = tagInt, C.integerValue = 1}
era = C.Int {C.integerBits = tagInt, C.integerValue = 2}
dup = C.Int {C.integerBits = tagInt, C.integerValue = 3}

eacList ∷ Type.Type
eacList = Type.StructureType
  { -- change to true later?
    Type.isPacked = False,
    Type.elementTypes = [eacPointer, eacLPointer]
  }

eacLPointer ∷ Type.Type
eacLPointer = Type.PointerType eacList (Addr.AddrSpace 32)

loadCar,
  loadCdr ∷
    ( HasThrow "err" Codegen.Errors m,
      HasState "blocks" (Map.HashMap Name.Name Codegen.BlockState) m,
      HasState "count" Word m,
      HasState "currentBlock" Name.Name m
    ) ⇒
    Operand.Operand →
    m Operand.Operand
loadCar eacList = do
  eacPointer ←
    Codegen.loadElementPtr $
      Codegen.Minimal
        { Codegen.type' = eacPointer,
          Codegen.address' = eacList,
          Codegen.indincies' = Codegen.constant32List [0, 0]
        }
  Codegen.load eac eacPointer
loadCdr eacList = do
  eacPointer ←
    Codegen.loadElementPtr $
      Codegen.Minimal
        { Codegen.type' = eacLPointer,
          Codegen.address' = eacList,
          Codegen.indincies' = Codegen.constant32List [0, 1]
        }
  Codegen.load eac eacPointer
