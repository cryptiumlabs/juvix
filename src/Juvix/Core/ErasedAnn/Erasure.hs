module Juvix.Core.ErasedAnn.Erasure where

import qualified Juvix.Core.Erased.Types as E
import qualified Juvix.Core.ErasedAnn.Types as Types
import Juvix.Core.ErasedAnn.Types (AnnTerm (..))
import Juvix.Library

eraseTerm ∷ Types.Term primTy primVal → E.Term primVal
eraseTerm term =
  case term of
    Types.Var s → E.Var s
    Types.Prim p → E.Prim p
    Types.LamM _ args (Ann bod _ _) →
      foldr E.Lam (eraseTerm bod) args
    Types.AppM (Ann f _ _) xs →
      foldl (\apps (Ann x _ _) → E.App apps (eraseTerm x)) (eraseTerm f) xs
