-- |
-- Serves as a mini DSL layer above LLVM
-- * What is included?
-- 1. _Relink_
--    - gives a declarative way to do a bunch of links and relinks
module Juvix.Backends.LLVM.DSL where

import qualified Juvix.Backends.LLVM.Codegen as Codegen
import Juvix.Library hiding (reduce)
import qualified Juvix.Library.HashMap as Map
import qualified LLVM.AST.Name as Name
import qualified LLVM.AST.Operand as Operand
import qualified LLVM.AST.Type as Type
import Prelude (error)

-- | Type for specifying how one wants to link nodes
-- inspired from the interpreter version.
data Relink node port
  = RelAuxiliary
      { node ∷ node,
        primary ∷ REL node port,
        auxiliary1 ∷ REL node port,
        auxiliary2 ∷ REL node port,
        auxiliary3 ∷ REL node port,
        auxiliary4 ∷ REL node port
      }

defRel ∷ Relink node port
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

auxiliaryToPort ∷
  ( HasThrow "err" Codegen.Errors m,
    HasState "blocks" (Map.HashMap Name.Name Codegen.BlockState) m,
    HasState "count" Word m,
    HasState "currentBlock" Name.Name m,
    HasState "typTab" Codegen.TypeTable m,
    HasState "varTab" Codegen.VariantToType m
  ) ⇒
  Auxiliary →
  Type.Type →
  m Operand.Operand
auxiliaryToPort Prim = Codegen.mainPort
auxiliaryToPort Aux1 = Codegen.auxiliary1
auxiliaryToPort Aux2 = Codegen.auxiliary2
auxiliaryToPort Aux3 = Codegen.auxiliary3
auxiliaryToPort Aux4 = Codegen.auxiliary4

linkAll ∷
  ( HasThrow "err" Codegen.Errors f,
    HasState "blocks" (Map.HashMap Name.Name Codegen.BlockState) f,
    HasState "count" Word f,
    HasState "currentBlock" Name.Name f,
    HasState "symtab" Codegen.SymbolTable f,
    HasState "typTab" Codegen.TypeTable f,
    HasState "varTab" Codegen.VariantToType f
  ) ⇒
  Type.Type →
  Relink Operand.Operand Auxiliary →
  f ()
linkAll t (RelAuxiliary node p a1 a2 a3 a4) = do
  -- Reodoing Codegen.mainPort/auxiliary* may or may not have an extra cost.
  -- TODO ∷ if it does, make them once at the top level and pass them around in the env!
  let flipHelper p l = linkHelper t l node p
  flipHelper (Codegen.mainPort t) p
  flipHelper (Codegen.auxiliary1 t) a1
  flipHelper (Codegen.auxiliary2 t) a2
  flipHelper (Codegen.auxiliary3 t) a3
  flipHelper (Codegen.auxiliary4 t) a4

linkHelper ∷
  ( HasThrow "err" Codegen.Errors m,
    HasState "blocks" (Map.HashMap Name.Name Codegen.BlockState) m,
    HasState "count" Word m,
    HasState "currentBlock" Name.Name m,
    HasState "symtab" Codegen.SymbolTable m,
    HasState "typTab" Codegen.TypeTable m,
    HasState "varTab" Codegen.VariantToType m
  ) ⇒
  Type.Type →
  REL Operand.Operand Auxiliary →
  Operand.Operand →
  m Operand.Operand →
  m ()
linkHelper _ None _ _ =
  pure ()
linkHelper ty (Link nl pl) node port = do
  -- TODO ∷ see if this extra "allocation" actually is costly
  port ← port
  p ← auxiliaryToPort pl ty
  Codegen.link [node, port, nl, p]
linkHelper ty (LinkConnected nl pl) node port = do
  -- TODO ∷ see if this extra "allocation" actually is costly
  port ← port
  p ← auxiliaryToPort pl ty
  Codegen.linkConnectedPort [nl, p, node, port]
