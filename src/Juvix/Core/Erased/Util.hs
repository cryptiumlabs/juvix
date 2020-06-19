module Juvix.Core.Erased.Util where

import qualified Data.Set as Set
import Juvix.Core.Erased.Types
import Juvix.Library

freeTerm :: forall primTy primVal. Term primTy primVal -> Set.Set Symbol
freeTerm term =
  let go :: Set.Set Symbol -> Term primTy primVal -> Set.Set Symbol
      go used t =
        case t of
          Lam v b -> go (Set.insert v used) b
          Elim e -> used `Set.union` freeElim e
   in go Set.empty term

freeElim :: forall primTy primVal. Elim primTy primVal -> Set.Set Symbol
freeElim elim =
  let go :: Set.Set Symbol -> Elim primTy primVal -> Set.Set Symbol
      go used e =
        case e of
          Prim _ -> Set.empty
          Var s -> if Set.member s used then Set.empty else Set.singleton s
          App a b -> used `Set.union` freeElim a `Set.union` freeTerm b
  in go Set.empty elim
