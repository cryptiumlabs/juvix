module Juvix.Core.IRAnn.Erasure where

import qualified Juvix.Core.IR.Types as IR
import Juvix.Core.IRAnn.Types

eraseTerm ∷ ∀ primTy primVal. AnnTerm primTy primVal → IR.Term primTy primVal
eraseTerm (term, _, _) =
  case term of
    Star n → IR.Star n
    PrimTy t → IR.PrimTy t
    Pi u a b → IR.Pi u (eraseTerm a) (eraseTerm b)
    Lam b → IR.Lam (eraseTerm b)
    Elim e → IR.Elim (eraseElim e)

eraseElim ∷ ∀ primTy primVal. AnnElim primTy primVal → IR.Elim primTy primVal
eraseElim (elim, _, _) =
  case elim of
    Bound s → IR.Bound s
    Free n → IR.Free n
    Prim p → IR.Prim p
    App f x → IR.App (eraseElim f) (eraseTerm x)
    Ann u x y → IR.Ann u (eraseTerm x) (eraseTerm y)
