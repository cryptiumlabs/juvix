module Juvix.Core.HRAnn.Erasure where

import qualified Juvix.Core.HR.Types as HR
import qualified Juvix.Core.HRAnn.Types as HRAnn
import Juvix.Core.IR.TransformExt
import Juvix.Library

hrForgetter :: ExtTransformTE HRAnn.T HR.T primTy primVal
hrForgetter =
  ExtTransformTE
    { etStar = identity,
      etPrimTy = identity,
      etPi = identity,
      etLam = HRAnn.bindName,
      etSig = identity,
      etPair = identity,
      etLet = HRAnn.letName,
      etElim = const (),
      etBound = absurd,
      etFree = absurd,
      etPrim = identity,
      etApp = const (),
      etAnn = identity,
      etTermX = absurd,
      etElimX = identity
    }

eraseTerm :: forall primTy primVal. HRAnn.Term primTy primVal -> HR.Term primTy primVal
eraseTerm = extTransformT hrForgetter

eraseElim :: forall primTy primVal. HRAnn.Elim primTy primVal -> HR.Elim primTy primVal
eraseElim = extTransformE hrForgetter
