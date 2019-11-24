module Juvix.Core.ErasedAnn.Erasure where

import qualified Juvix.Core.Erased.Types as E
import Juvix.Core.ErasedAnn.Types

eraseTerm ∷ ∀ primTy primVal. AnnTerm primTy primVal → E.Term primVal
eraseTerm (term, _, _) =
  case term of
    Var s → E.Var s
    Prim p → E.Prim p
    Lam s b → E.Lam s (eraseTerm b)
    App f x → E.App (eraseTerm f) (eraseTerm x)
