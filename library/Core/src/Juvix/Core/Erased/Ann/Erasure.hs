module Juvix.Core.Erased.Ann.Erasure where

import Juvix.Core.Erased.Ann.Types (AnnTerm (..))
import qualified Juvix.Core.Erased.Ann.Types as Types
import qualified Juvix.Core.Erased.Types as E
import Juvix.Library

eraseTerm :: Types.Term primTy primVal -> E.Term primVal
eraseTerm term =
  case term of
    Types.Var s -> E.Var s
    Types.Prim p -> E.Prim p
    Types.LamM _ args (Ann _ _ bod) ->
      foldr E.Lam (eraseTerm bod) args
    Types.PairM (Ann _ _ left) (Ann _ _ right) ->
      E.Pair (eraseTerm left) (eraseTerm right)
    Types.UnitM ->
      E.Unit
    Types.AppM (Ann _ _ f) xs ->
      foldl (\apps (Ann _ _ x) -> E.App apps (eraseTerm x)) (eraseTerm f) xs
