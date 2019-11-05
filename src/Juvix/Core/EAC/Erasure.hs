module Juvix.Core.EAC.Erasure
  ( erase,
  )
where

import Juvix.Core.EAC.Types
import Juvix.Core.Erased.Types

erase ∷ ∀ primVal. RPTO primVal → Term primVal
erase (RBang _ inner) = eraseInner inner

eraseInner ∷ ∀ primVal. RPTI primVal → Term primVal
eraseInner term =
  case term of
    RVar s → Var s
    RPrim p → Prim p
    RLam v b → Lam v (erase b)
    RApp f x → App (erase f) (erase x)
