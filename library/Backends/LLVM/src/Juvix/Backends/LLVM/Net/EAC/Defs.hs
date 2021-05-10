-- |
-- - Specializes the functions in Graph to fit [[Net/EAC/Types]]
--   + Later in the DSL Layer!
-- - Generates the =find_edge= and =isBothPrimary= function with the =eal= type.
-- - Also generates the proper types associated with them
-- - Also has some miscellaneous debug information
module Juvix.Backends.LLVM.Net.EAC.Defs where

import qualified Juvix.Backends.LLVM.Codegen as Codegen
import qualified Juvix.Backends.LLVM.DSL as DSL
import qualified Juvix.Backends.LLVM.Net.EAC.Types as Types
import Juvix.Library
import qualified LLVM.AST.Operand as Operand
import qualified LLVM.AST.Type as Type

--------------------------------------------------------------------------------
-- Extra Definitions
--------------------------------------------------------------------------------

printList :: Codegen.Call m => [Operand.Operand] -> m ()
printList args =
  Codegen.callGenVoid args "print_list"

definePrintList :: Codegen.Define m => m Operand.Operand
definePrintList = Codegen.defineFunction Type.void "print_list" args $ do
  _ <- Codegen.printCString "[" []
  listPtr <- Codegen.externf "list_ptr"
  printInner <- Codegen.externf "print_list_inner"
  _ <- Codegen.callVoid printInner (Codegen.emptyArgs [listPtr])
  Codegen.retNull
  where
    args = [(Types.eacLPointer, "list_ptr")]

definePrintListInner :: Codegen.Define m => m Operand.Operand
definePrintListInner = Codegen.defineFunction Type.void "print_list_inner" args $
  do
    listPtr <- Codegen.externf "list_ptr"
    existsCase <- Codegen.addBlock "exists.case"
    exitCase <- Codegen.addBlock "exit.case"
    nullCheck <- Types.checkNull listPtr
    _ <- Codegen.cbr nullCheck existsCase exitCase
    -- %exists.case branch
    ------------------------------------------------------
    Codegen.setBlock existsCase
    car <- Types.loadCar listPtr
    _ <- Codegen.printCString " %p, " [car]
    printInner <- Codegen.externf "print_list_inner"
    cdr <- Types.loadCdr listPtr
    _ <- Codegen.callVoid printInner (Codegen.emptyArgs [cdr])
    _ <- Codegen.retNull
    -- %exit.case branch
    ------------------------------------------------------
    _ <- Codegen.setBlock exitCase
    _ <- Codegen.printCString "] \n" []
    Codegen.retNull
  where
    args = [(Types.eacLPointer, "list_ptr")]

--------------------------------------------------------------------------------
-- Aliases
--------------------------------------------------------------------------------
isBothPrimary :: Codegen.Call m => [Operand.Operand] -> m Operand.Operand
isBothPrimary = Codegen.isBothPrimary

findEdge :: Codegen.Call m => [Operand.Operand] -> m Operand.Operand
findEdge = Codegen.findEdge

bothPrimary :: Type.Type
bothPrimary = Codegen.bothPrimary

portType :: Type.Type
portType = Codegen.portType Types.eacPointer

mainPort,
  auxiliary1,
  auxiliary2,
  auxiliary3,
  auxiliary4 ::
    Codegen.Externf m => m Operand.Operand
mainPort = Codegen.mainPort
auxiliary1 = Codegen.auxiliary1
auxiliary2 = Codegen.auxiliary2
auxiliary3 = Codegen.auxiliary3
auxiliary4 = Codegen.auxiliary4

linkAll ::
  Codegen.Call f => DSL.Relink Operand.Operand Operand.Operand DSL.Auxiliary -> f ()
linkAll = DSL.linkAll

linkAllCons ::
  ( Codegen.Call m,
    Codegen.NewBlock m,
    Codegen.MallocNode m
  ) =>
  Operand.Operand ->
  DSL.Relink
    (DSL.Node Operand.Operand Operand.Operand)
    Operand.Operand
    DSL.Auxiliary ->
  m Operand.Operand
linkAllCons = DSL.linkAllCons Types.cons Types.eacLPointer

loadPrimaryNode :: Codegen.RetInstruction m => Operand.Operand -> m Operand.Operand
loadPrimaryNode = Codegen.loadPrimaryNode Types.eacPointer

--------------------------------------------------------------------------------
-- Graph operation definitions
--------------------------------------------------------------------------------

mallocNodeH ::
  ( Codegen.RetInstruction m,
    Codegen.Debug m,
    HasState "typTab" Codegen.TypeTable m,
    HasState "varTab" Codegen.VariantToType m,
    HasState "symTab" Codegen.SymbolTable m
  ) =>
  [Maybe Operand.Operand] ->
  [Maybe Operand.Operand] ->
  m Operand.Operand
mallocNodeH mPorts mData = Codegen.mallocNodeH mPorts mData 4

defineIsBothPrimary :: (Codegen.Define m, Codegen.Debug m) => m Operand.Operand
defineIsBothPrimary = Codegen.defineIsBothPrimary

defineRewire :: Codegen.Define m => m Operand.Operand
defineRewire = Codegen.defineRewire

defineLinkConnectedPort :: Codegen.Define m => m Operand.Operand
defineLinkConnectedPort = Codegen.defineLinkConnectedPort
