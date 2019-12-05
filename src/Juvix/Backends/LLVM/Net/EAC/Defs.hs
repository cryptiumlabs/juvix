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
import qualified Juvix.Library.HashMap as Map
import qualified LLVM.AST as AST
import qualified LLVM.AST.Name as Name
import qualified LLVM.AST.Operand as Operand
import qualified LLVM.AST.Type as Type

--------------------------------------------------------------------------------
-- Aliases
--------------------------------------------------------------------------------
isBothPrimary,
  findEdge ∷
    ( HasThrow "err" Codegen.Errors m,
      HasState "blocks" (Map.HashMap Name.Name Codegen.BlockState) m,
      HasState "count" Word m,
      HasState "currentBlock" Name.Name m,
      HasState "symtab" Codegen.SymbolTable m
    ) ⇒
    [Operand.Operand] →
    m Operand.Operand
isBothPrimary = Codegen.isBothPrimary Types.eacPointer
findEdge = Codegen.findEdge Types.eacPointer

bothPrimary ∷ Type.Type
bothPrimary = Codegen.bothPrimary Types.eacPointer

mainPort,
  auxiliary1,
  auxiliary2,
  auxiliary3,
  auxiliary4 ∷
    ( HasThrow "err" Codegen.Errors m,
      HasState "blocks" (Map.HashMap Name.Name Codegen.BlockState) m,
      HasState "count" Word m,
      HasState "currentBlock" Name.Name m,
      HasState "typTab" Codegen.TypeTable m,
      HasState "varTab" Codegen.VariantToType m
    ) ⇒
    m Operand.Operand
mainPort = Codegen.mainPort Types.eacPointer
auxiliary1 = Codegen.auxiliary1 Types.eacPointer
auxiliary2 = Codegen.auxiliary2 Types.eacPointer
auxiliary3 = Codegen.auxiliary3 Types.eacPointer
auxiliary4 = Codegen.auxiliary4 Types.eacPointer

linkAll ∷
  ( HasThrow "err" Codegen.Errors f,
    HasState "blocks" (Map.HashMap Name.Name Codegen.BlockState) f,
    HasState "count" Word f,
    HasState "currentBlock" Name.Name f,
    HasState "symtab" Codegen.SymbolTable f,
    HasState "typTab" Codegen.TypeTable f,
    HasState "varTab" Codegen.VariantToType f
  ) ⇒
  DSL.Relink Operand.Operand DSL.Auxiliary →
  f ()
linkAll = DSL.linkAll Types.eacPointer

nodeType ∷ Type.Type
nodeType = Codegen.nodeType Types.eacPointer

allocaNodeH ∷
  ( HasThrow "err" Codegen.Errors m,
    HasState "blocks" (Map.HashMap Name.Name Codegen.BlockState) m,
    HasState "count" Word m,
    HasState "currentBlock" Name.Name m,
    HasState "typTab" Codegen.TypeTable m,
    HasState "varTab" Codegen.VariantToType m
  ) ⇒
  [Maybe Operand.Operand] →
  [Maybe Operand.Operand] →
  m Operand.Operand
allocaNodeH xs ys = Codegen.allocaNodeH xs ys Types.eacPointer

loadPrimaryNode ∷
  ( HasThrow "err" Codegen.Errors m,
    HasState "blocks" (Map.HashMap Name.Name Codegen.BlockState) m,
    HasState "count" Word m,
    HasState "currentBlock" Name.Name m
  ) ⇒
  Operand.Operand →
  m Operand.Operand
loadPrimaryNode = Codegen.loadPrimaryNode Types.eacPointer

--------------------------------------------------------------------------------
-- Graph operation definitions
--------------------------------------------------------------------------------

isBothPrimary' ∷
  ( HasThrow "err" Codegen.Errors m,
    HasState "blockCount" Int m,
    HasState "blocks" (Map.HashMap Name.Name Codegen.BlockState) m,
    HasState "count" Word m,
    HasState "currentBlock" Name.Name m,
    HasState "moduleDefinitions" [AST.Definition] m,
    HasState "names" Codegen.Names m,
    HasState "symtab" (Map.HashMap Symbol Operand.Operand) m,
    HasState "typTab" Codegen.TypeTable m,
    HasState "varTab" Codegen.VariantToType m
  ) ⇒
  m Operand.Operand
isBothPrimary' = Codegen.isBothPrimary' Types.eacPointer

findEdge' ∷
  ( HasThrow "err" Codegen.Errors m,
    HasState "blockCount" Int m,
    HasState "blocks" (Map.HashMap Name.Name Codegen.BlockState) m,
    HasState "count" Word m,
    HasState "currentBlock" Name.Name m,
    HasState "moduleDefinitions" [AST.Definition] m,
    HasState "names" Codegen.Names m,
    HasState "symtab" (Map.HashMap Symbol Operand.Operand) m
  ) ⇒
  m Operand.Operand
findEdge' = Codegen.findEdge' Types.eacPointer

link' ∷
  ( HasThrow "err" Codegen.Errors m,
    HasState "blockCount" Int m,
    HasState "blocks" (Map.HashMap Name.Name Codegen.BlockState) m,
    HasState "count" Word m,
    HasState "currentBlock" Name.Name m,
    HasState "moduleDefinitions" [AST.Definition] m,
    HasState "names" Codegen.Names m,
    HasState "symtab" (Map.HashMap Symbol Operand.Operand) m
  ) ⇒
  m Operand.Operand
link' = Codegen.link' Types.eacPointer

rewire' ∷
  ( HasThrow "err" Codegen.Errors m,
    HasState "blockCount" Int m,
    HasState "blocks" (Map.HashMap Name.Name Codegen.BlockState) m,
    HasState "count" Word m,
    HasState "currentBlock" Name.Name m,
    HasState "moduleDefinitions" [AST.Definition] m,
    HasState "names" Codegen.Names m,
    HasState "symtab" Codegen.SymbolTable m
  ) ⇒
  m Operand.Operand
rewire' = Codegen.rewire' Types.eacPointer

linkConnectedPort' ∷
  ( HasThrow "err" Codegen.Errors m,
    HasState "blockCount" Int m,
    HasState "blocks" (Map.HashMap Name.Name Codegen.BlockState) m,
    HasState "count" Word m,
    HasState "currentBlock" Name.Name m,
    HasState "moduleDefinitions" [AST.Definition] m,
    HasState "names" Codegen.Names m,
    HasState "symtab" Codegen.SymbolTable m
  ) ⇒
  m Operand.Operand
linkConnectedPort' = Codegen.linkConnectedPort' Types.eacPointer
