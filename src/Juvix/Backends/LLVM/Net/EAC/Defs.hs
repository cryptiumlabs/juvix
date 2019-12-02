-- |
-- - Specializes the functions in Graph to fit [[Net/EAC/Types]]
--   + Later in the DSL Layer!
-- - Generates the =find_edge= and =isBothPrimary= function with the =eal= type.
-- - Also generates the proper types associated with them
module Juvix.Backends.LLVM.Net.EAC.Defs where

import qualified Juvix.Backends.LLVM.Codegen as Codegen
import qualified Juvix.Backends.LLVM.Net.EAC.Types as Types
import Juvix.Library
import qualified Juvix.Library.HashMap as Map
import qualified LLVM.AST as AST
import qualified LLVM.AST.Name as Name
import qualified LLVM.AST.Operand as Operand
import qualified LLVM.AST.Type as Type

isBothPrimary ∷
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
isBothPrimary = Codegen.isBothPrimary Types.eacPointer

findEdge ∷
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
findEdge = Codegen.findEdge Types.eacPointer

bothPrimary ∷ Type.Type
bothPrimary = Codegen.bothPrimary Types.eacPointer
