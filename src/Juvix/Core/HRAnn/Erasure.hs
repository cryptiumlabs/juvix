module Juvix.Core.HRAnn.Erasure where

import Juvix.Library
import qualified Juvix.Core.HR.Types as HR
import Juvix.Core.HRAnn.Types
import Juvix.Core.IR.TransformExt


hrForgetter :: ExtTransformTE HRAnn HR.HR primTy primVal
hrForgetter =
  ExtTransformTE {
    etStar   = identity,
    etPrimTy = identity,
    etPi     = identity,
    etLam    = fst,
    etElim   = const (),

    etBound = absurd,
    etFree  = absurd,
    etPrim  = identity,
    etApp   = const (),
    etAnn   = identity,

    etTermX = absurd,
    etElimX = identity
  }

eraseTerm ∷ ∀ primTy primVal. Term primTy primVal → HR.Term primTy primVal
eraseTerm = extTransformT hrForgetter

eraseElim ∷ ∀ primTy primVal. Elim primTy primVal → HR.Elim primTy primVal
eraseElim = extTransformE hrForgetter
