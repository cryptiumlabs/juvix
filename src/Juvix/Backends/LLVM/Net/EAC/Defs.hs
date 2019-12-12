-- |
-- - Specializes the functions in Graph to fit [[Net/EAC/Types]]
--   + Later in the DSL Layer!
-- - Generates the =find_edge= and =isBothPrimary= function with the =eal= type.
-- - Also generates the proper types associated with them
module Juvix.Backends.LLVM.Net.EAC.Defs where

import qualified Juvix.Backends.LLVM.Codegen as Codegen
import qualified Juvix.Backends.LLVM.DSL as DSL
import qualified Juvix.Backends.LLVM.Net.EAC.Types as Types
import Juvix.Library
import qualified LLVM.AST.Operand as Operand
import qualified LLVM.AST.Type as Type

--------------------------------------------------------------------------------
-- Aliases
--------------------------------------------------------------------------------
isBothPrimary ∷ Codegen.Call m ⇒ [Operand.Operand] → m Operand.Operand
isBothPrimary = Codegen.isBothPrimary Types.eacPointer

findEdge ∷ Codegen.Call m ⇒ [Operand.Operand] → m Operand.Operand
findEdge = Codegen.findEdge Types.eacPointer

bothPrimary ∷ Type.Type
bothPrimary = Codegen.bothPrimary Types.eacPointer

mainPort,
  auxiliary1,
  auxiliary2,
  auxiliary3,
  auxiliary4 ∷
    ( HasState "symTab" Codegen.SymbolTable m,
      HasThrow "err" Codegen.Errors m
    ) ⇒
    m Operand.Operand
mainPort = Codegen.mainPort
auxiliary1 = Codegen.auxiliary1
auxiliary2 = Codegen.auxiliary2
auxiliary3 = Codegen.auxiliary3
auxiliary4 = Codegen.auxiliary4

linkAll ∷
  Codegen.Call f ⇒ DSL.Relink Operand.Operand DSL.Auxiliary → f ()
linkAll = DSL.linkAll

nodeType ∷ Type.Type
nodeType = Codegen.nodeType Types.eacPointer

nodePointer ∷ Type.Type
nodePointer = Codegen.nodePointer Types.eacPointer

mallocNodeH ∷
  ( Codegen.RetInstruction m,
    HasState "typTab" Codegen.TypeTable m,
    HasState "varTab" Codegen.VariantToType m,
    HasState "symTab" Codegen.SymbolTable m
  ) ⇒
  [Maybe Operand.Operand] →
  [Maybe Operand.Operand] →
  m Operand.Operand
mallocNodeH xs ys =
  Codegen.mallocNodeH xs ys Types.eacPointer

loadPrimaryNode ∷ Codegen.RetInstruction m ⇒ Operand.Operand → m Operand.Operand
loadPrimaryNode = Codegen.loadPrimaryNode Types.eacPointer

--------------------------------------------------------------------------------
-- Graph operation definitions
--------------------------------------------------------------------------------

defineIsBothPrimary ∷ Codegen.Define m ⇒ m Operand.Operand
defineIsBothPrimary = Codegen.defineIsBothPrimary Types.eacPointer

defineFindEdge ∷ Codegen.Define m ⇒ m Operand.Operand
defineFindEdge = Codegen.defineFindEdge Types.eacPointer

defineLink ∷ Codegen.Define m ⇒ m Operand.Operand
defineLink = Codegen.defineLink Types.eacPointer

defineRewire ∷ Codegen.Define m ⇒ m Operand.Operand
defineRewire = loadPtrGen Codegen.defineRewire

defineLinkConnectedPort ∷ Codegen.Define m ⇒ m Operand.Operand
defineLinkConnectedPort = loadPtrGen Codegen.defineLinkConnectedPort

deAllocateNode ∷ Codegen.Define m ⇒ Operand.Operand → m Operand.Operand
deAllocateNode nodePtr = Codegen.deAllocateNode nodePtr

loadPtrGen ∷ Codegen.Define m ⇒ (Type.Type → (Operand.Operand → m Operand.Operand) → t) → t
loadPtrGen f =
  f
    Types.eacPointer
    $ \eac → do
      Codegen.loadElementPtr $
        Codegen.Minimal
          { Codegen.type' = nodePointer,
            Codegen.address' = eac,
            Codegen.indincies' = Codegen.constant32List [0, 0]
          }
