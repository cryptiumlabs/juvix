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
import Prelude (error)

-- | Type for specifying how one wants to link nodes
-- inspired from the interpreter version.
data Relink node port
  = RelAuxiliary
      { node ∷ node,
        primary ∷ Maybe (REL node port),
        auxiliary1 ∷ Maybe (REL node port),
        auxiliary2 ∷ Maybe (REL node port),
        auxiliary3 ∷ Maybe (REL node port),
        auxiliary4 ∷ Maybe (REL node port)
      }

defRel ∷ Relink node port
defRel =
  RelAuxiliary
    (error "put in default node into relAuxiliary")
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing

-- | REL: a type that displays whether we are linking from an old node or just adding a new link
data REL node port
  = Link node port
  | LinkConnected node port
  deriving (Show)

linkAll ∷
  ( HasThrow "err" Codegen.Errors f,
    HasState "blocks" (Map.HashMap Name.Name Codegen.BlockState) f,
    HasState "count" Word f,
    HasState "currentBlock" Name.Name f,
    HasState "symtab" Codegen.SymbolTable f,
    HasState "typTab" Codegen.TypeTable f,
    HasState "varTab" Codegen.VariantToType f
  ) ⇒
  Relink Operand.Operand Operand.Operand →
  f ()
linkAll (RelAuxiliary node p a1 a2 a3 a4) = do
  -- Reodoing Codegen.mainPort/auxiliary* may or may not have an extra cost.
  -- Find out!
  let flipHelper p f = p >>= linkHelper f node
  traverse_ (flipHelper Codegen.mainPort) p
  traverse_ (flipHelper Codegen.auxiliary1) a1
  traverse_ (flipHelper Codegen.auxiliary2) a2
  traverse_ (flipHelper Codegen.auxiliary3) a3
  traverse_ (flipHelper Codegen.auxiliary4) a4

linkHelper ∷
  ( HasThrow "err" Codegen.Errors m,
    HasState "blocks" (Map.HashMap Name.Name Codegen.BlockState) m,
    HasState "count" Word m,
    HasState "currentBlock" Name.Name m,
    HasState "symtab" Codegen.SymbolTable m
  ) ⇒
  REL Operand.Operand Operand.Operand →
  Operand.Operand →
  Operand.Operand →
  m ()
linkHelper (Link nl pl) node port =
  Codegen.link [node, port, nl, pl]
linkHelper (LinkConnected nl pl) node port =
  Codegen.linkConnectedPort [nl, pl, node, port]
