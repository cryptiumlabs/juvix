module Juvix.Core.ErasedAnn.Erasure where

import qualified Juvix.Core.Erased.Types as E
import Juvix.Core.ErasedAnn.Types

eraseTerm ∷ ∀ primTy primVal. Term primTy primVal → E.Term primVal
eraseTerm term =
  case term of
    Var s → E.Var s
    Prim p → E.Prim p
    Lam s (b, _, _) → E.Lam s (eraseTerm b)
    App (f, _, _) (x, _, _) → E.App (eraseTerm f) (eraseTerm x)
