module Juvix.Core.Erased.Util where

import qualified Data.Set as Set
import Juvix.Core.Erased.Types
import Juvix.Library

free :: forall primVal. Term primVal -> [Symbol]
free term =
  let go used t =
        case t of
          Var s -> if Set.member s used then Set.empty else Set.singleton s
          Prim _ -> Set.empty
          Lam v b -> go (Set.insert v used) b
          App a b -> go used a `Set.union` go used b
   in Set.toList (go Set.empty term)
