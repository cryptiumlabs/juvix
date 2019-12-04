module Juvix.Core.IRAnn.Erasure where

import qualified Juvix.Core.IR.Types as IR
import Juvix.Core.IRAnn.Types

eraseTerm ∷ ∀ primTy primVal. Term primTy primVal → IR.Term primTy primVal
eraseTerm term =
  case term of
    Star n → IR.Star n
    PrimTy t → IR.PrimTy t
    Pi u a b → IR.Pi u (eraseTerm a) (eraseTerm b)
    Lam (b, _, _) → IR.Lam (eraseTerm b)
    Elim (e, _, _) → IR.Elim (eraseElim e)

eraseElim ∷ ∀ primTy primVal. Elim primTy primVal → IR.Elim primTy primVal
eraseElim elim =
  case elim of
    Bound s → IR.Bound s
    Free n → IR.Free n
    Prim p → IR.Prim p
    App (f, _, _) (x, _, _) → IR.App (eraseElim f) (eraseTerm x)
    Ann u x y → IR.Ann u (eraseTerm x) (eraseTerm y)
