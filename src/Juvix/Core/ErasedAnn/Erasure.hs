module Juvix.Core.ErasedAnn.Erasure where

import qualified Juvix.Core.Erased.Types as E
import Juvix.Core.ErasedAnn.Types
import Juvix.Library

eraseTerm ∷ ∀ primTy primVal. Term primTy primVal → E.Term primVal
eraseTerm term =
  case term of
    Var s → E.Var s
    Prim p → E.Prim p
    -- TODO ∷ remove, as normal lam is useless!
    Lam s (b, _, _) → E.Lam s (eraseTerm b)
    -- TODO ∷ add proper lam to be erased to!
    LamM _ args (bod, _, _) → foldr E.Lam (eraseTerm bod) args
    App (f, _, _) (x, _, _) → E.App (eraseTerm f) (eraseTerm x)
