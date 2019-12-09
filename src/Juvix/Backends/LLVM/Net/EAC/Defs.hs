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
  Codegen.mallocNodeH xs ys Types.eacPointer (Types.tagInt + Codegen.nodePointerSize)

loadPrimaryNode ∷ Codegen.RetInstruction m ⇒ Operand.Operand → m Operand.Operand
loadPrimaryNode = Codegen.loadPrimaryNode Types.eacPointer

--------------------------------------------------------------------------------
-- Graph operation definitions
--------------------------------------------------------------------------------

isBothPrimary' ∷ Codegen.Define m ⇒ m Operand.Operand
isBothPrimary' = Codegen.isBothPrimary' Types.eacPointer

findEdge' ∷ Codegen.Define m ⇒ m Operand.Operand
findEdge' = Codegen.findEdge' Types.eacPointer

link' ∷ Codegen.Define m ⇒ m Operand.Operand
link' = Codegen.link' Types.eacPointer

rewire' ∷ Codegen.Define m ⇒ m Operand.Operand
rewire' = Codegen.rewire' Types.eacPointer

linkConnectedPort' ∷ Codegen.Define m ⇒ m Operand.Operand
linkConnectedPort' = Codegen.linkConnectedPort' Types.eacPointer
