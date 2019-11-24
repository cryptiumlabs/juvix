module Juvix.Core.HRAnn.Erasure where

import qualified Juvix.Core.HR.Types as HR
import Juvix.Core.HRAnn.Types

eraseTerm ∷ ∀ primTy primVal. AnnTerm primTy primVal → HR.Term primTy primVal
eraseTerm (term, _, _) =
  case term of
    Star n → HR.Star n
    PrimTy t → HR.PrimTy t
    Pi u a b → HR.Pi u (eraseTerm a) (eraseTerm b)
    Lam s b → HR.Lam s (eraseTerm b)
    Elim e → HR.Elim (eraseElim e)

eraseElim ∷ ∀ primTy primVal. AnnElim primTy primVal → HR.Elim primTy primVal
eraseElim (elim, _, _) =
  case elim of
    Var s → HR.Var s
    Prim p → HR.Prim p
    App f x → HR.App (eraseElim f) (eraseTerm x)
    Ann u x y → HR.Ann u (eraseTerm x) (eraseTerm y)
