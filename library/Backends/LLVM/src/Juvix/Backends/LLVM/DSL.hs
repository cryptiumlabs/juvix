-- |
-- Serves as a mini DSL layer above LLVM
-- * What is included?
-- 1. _Relink_
--    - gives a declarative way to do a bunch of links and relinks
module Juvix.Backends.LLVM.DSL where

import qualified Juvix.Backends.LLVM.Codegen as Codegen
import Juvix.Library hiding (reduce)
import qualified LLVM.AST.Operand as Operand
import qualified LLVM.AST.Type as Type
import Prelude (error)

-- | Type for specifying how one wants to link nodes
-- inspired from the interpreter version.
data Relink node' node port
  = RelAuxiliary
      -- hack fix later!
      { node :: node',
        primary :: REL node port,
        auxiliary1 :: REL node port,
        auxiliary2 :: REL node port,
        auxiliary3 :: REL node port,
        auxiliary4 :: REL node port
      }

data Node t n
  = Node
      { tagNode :: t,
        node' :: n
      }

defRel :: Relink node' node port
defRel =
  RelAuxiliary
    (error "put in default node into relAuxiliary")
    None
    None
    None
    None
    None

-- | REL: a type that displays whether we are linking from an old node or just adding a new link
data REL node port
  = Link node port
  | LinkConnected node port
  | None
  deriving (Show)

data Auxiliary = Prim | Aux1 | Aux2 | Aux3 | Aux4 deriving (Show, Eq)

auxiliaryToPort :: Codegen.Externf m => Auxiliary -> m Operand.Operand
auxiliaryToPort Prim = Codegen.mainPort
auxiliaryToPort Aux1 = Codegen.auxiliary1
auxiliaryToPort Aux2 = Codegen.auxiliary2
auxiliaryToPort Aux3 = Codegen.auxiliary3
auxiliaryToPort Aux4 = Codegen.auxiliary4

linkAllCons ::
  ( Codegen.Call m,
    Codegen.NewBlock m
  ) =>
  (t -> Operand.Operand -> m Operand.Operand) ->
  Type.Type ->
  Operand.Operand ->
  Relink (Node t Operand.Operand) Operand.Operand Auxiliary ->
  m Operand.Operand
linkAllCons cons eacListPtrType eacList (RelAuxiliary (Node tagN node) p a1 a2 a3 a4) = do
  let flipHelper p l = linkHelper l node p
  flipHelper Codegen.auxiliary1 a1
  flipHelper Codegen.auxiliary2 a2
  flipHelper Codegen.auxiliary3 a3
  flipHelper Codegen.auxiliary4 a4
  -- Always do this last
  linkHelperP cons eacListPtrType eacList p tagN node Codegen.mainPort

linkAll :: Codegen.Call m => Relink Operand.Operand Operand.Operand Auxiliary -> m ()
linkAll (RelAuxiliary node p a1 a2 a3 a4) = do
  let flipHelper p l = linkHelper l node p
  flipHelper Codegen.auxiliary1 a1
  flipHelper Codegen.auxiliary2 a2
  flipHelper Codegen.auxiliary3 a3
  flipHelper Codegen.auxiliary4 a4
  -- Always do this last
  flipHelper Codegen.mainPort p

linkHelper ::
  Codegen.Call f =>
  REL Operand.Operand Auxiliary ->
  Operand.Operand ->
  f Operand.Operand ->
  f ()
linkHelper None _ _ =
  pure ()
linkHelper (Link nl pl) node port = do
  port <- port
  p <- auxiliaryToPort pl
  Codegen.link [node, port, nl, p]
linkHelper (LinkConnected nl pl) node port = do
  port <- port
  p <- auxiliaryToPort pl
  Codegen.linkConnectedPort [nl, p, node, port]

linkHelperP ::
  ( Codegen.Call m,
    Codegen.NewBlock m
  ) =>
  (t -> Operand.Operand -> m Operand.Operand) ->
  Type.Type ->
  Operand.Operand ->
  REL Operand.Operand Auxiliary ->
  t ->
  Operand.Operand ->
  m Operand.Operand ->
  m Operand.Operand
linkHelperP cons eacListPtrType eacList n tagN node port =
  -- todo integrate this in a better way with link and link connected
  let consOnto = do
        v <- Codegen.isBothPrimary [node]
        primaryCase <- Codegen.addBlock "case.primary"
        continueComp <- Codegen.addBlock "case.continue"
        isPrimary <- Codegen.loadIsPrimaryEle v
        currentBlock <- Codegen.getBlock
        _ <- Codegen.cbr isPrimary primaryCase continueComp
        -- %case.primary branch
        ------------------------------------------------------
        Codegen.setBlock primaryCase
        newList <- cons tagN eacList
        _ <- Codegen.br continueComp
        -- %empty.continue branch
        ------------------------------------------------------
        Codegen.setBlock continueComp
        Codegen.phi eacListPtrType [(eacList, currentBlock), (newList, primaryCase)]
   in case n of
        None -> pure eacList
        Link nl pl -> do
          port <- port
          p <- auxiliaryToPort pl
          Codegen.link [node, port, nl, p]
          consOnto
        LinkConnected nl pl -> do
          port <- port
          p <- auxiliaryToPort pl
          Codegen.linkConnectedPort [nl, p, node, port]
          consOnto
