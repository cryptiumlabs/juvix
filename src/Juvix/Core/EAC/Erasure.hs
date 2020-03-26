module Juvix.Core.EAC.Erasure (erase) where

import qualified Juvix.Core.EAC.Types as EAC
import qualified Juvix.Core.Erased.Types as Types

erase ∷ ∀ primVal. EAC.RPTO primVal → Types.Term primVal
erase (EAC.RBang _ inner) = eraseInner inner

eraseInner ∷ ∀ primVal. EAC.RPTI primVal → Types.Term primVal
eraseInner term =
  case term of
    EAC.RVar s → Types.Var s
    EAC.RPrim p → Types.Prim p
    EAC.RLam v b → Types.Lam v (erase b)
    EAC.RApp f x → Types.App (erase f) (erase x)
