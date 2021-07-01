-- | A transformation that discards all annotations on term/elim nodes, but
-- keeps the extensions.
module Juvix.Core.Base.TransformExt.OnlyExts where

import Extensible hiding (Type)
import Juvix.Core.Base.TransformExt
import qualified Juvix.Core.Base.Types as Core
import Juvix.Library

data T (ext :: Type)

do
  ext' <- newName "ext"
  let ext = varT ext'
  decsT <- Core.extendTerm
    "Term"
    [ext']
    [t|T $ext|]
    \primTy primVal ->
      Core.defaultExtTerm
        { Core.typeTermX = [("TermX", [[t|Core.TermX $ext $primTy $primVal|]])]
        }
  decsE <- Core.extendElim
    "Elim"
    [ext']
    [t|T $ext|]
    \primTy primVal ->
      Core.defaultExtElim
        { Core.typeElimX = [("ElimX", [[t|Core.ElimX $ext $primTy $primVal|]])]
        }
  pure $ decsT <> decsE

onlyExtsT :: Core.Term' ext primTy primVal -> Core.Term' (T ext) primTy primVal
onlyExtsT = extTransformT transformer

onlyExtsE :: Core.Elim' ext primTy primVal -> Core.Elim' (T ext) primTy primVal
onlyExtsE = extTransformE transformer

transformer :: ExtTransformTE ext (T ext) primTy primVal
transformer =
  ExtTransformTE
    { etStar = const (),
      etPrimTy = const (),
      etPrim = const (),
      etPi = const (),
      etSig = const (),
      etPair = const (),
      etUnitTy = const (),
      etUnit = const (),
      etLam = const (),
      etLet = const (),
      etElim = const (),
      etBound = const (),
      etFree = const (),
      etApp = const (),
      etAnn = const (),
      etTermX = identity,
      etElimX = identity
    }

injectT ::
  ( Core.TermX ext' primTy primVal ~ Void,
    Core.ElimX ext' primTy primVal ~ Void,
    ForgotExt ext' primTy primVal
  ) =>
  Core.Term' ext' primTy primVal ->
  Core.Term' (T ext) primTy primVal
injectT = extTransformT injector

injectE ::
  ( Core.TermX ext' primTy primVal ~ Void,
    Core.ElimX ext' primTy primVal ~ Void,
    ForgotExt ext' primTy primVal
  ) =>
  Core.Elim' ext' primTy primVal ->
  Core.Elim' (T ext) primTy primVal
injectE = extTransformE injector

injector ::
  ( Core.TermX ext' primTy primVal ~ Void,
    Core.ElimX ext' primTy primVal ~ Void,
    ForgotExt ext' primTy primVal
  ) =>
  ExtTransformTE ext' (T ext) primTy primVal
injector =
  ExtTransformTE
    { etStar = identity,
      etPrimTy = identity,
      etPrim = identity,
      etPi = identity,
      etSig = identity,
      etPair = identity,
      etUnitTy = identity,
      etUnit = identity,
      etLam = identity,
      etLet = identity,
      etElim = identity,
      etBound = identity,
      etFree = identity,
      etApp = identity,
      etAnn = identity,
      etTermX = absurd,
      etElimX = absurd
    }
