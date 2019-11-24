module Juvix.Core.HRAnn.Erasure where

import qualified Juvix.Core.HR.Types as HR
import Juvix.Core.HRAnn.Types

eraseTerm ∷ ∀ primTy primVal. Term primTy primVal → HR.Term primTy primVal
eraseTerm term =
  case term of
    Star n → HR.Star n
    PrimTy t → HR.PrimTy t
    Pi u a b → HR.Pi u (eraseTerm a) (eraseTerm b)
    Lam s b → HR.Lam s (eraseTerm b)
    Elim (e, _, _) → HR.Elim (eraseElim e)

eraseElim ∷ ∀ primTy primVal. Elim primTy primVal → HR.Elim primTy primVal
eraseElim elim =
  case elim of
    Var s → HR.Var s
    Prim p → HR.Prim p
    App (f, _, _) x → HR.App (eraseElim f) (eraseTerm x)
    Ann u x y → HR.Ann u (eraseTerm x) (eraseTerm y)
