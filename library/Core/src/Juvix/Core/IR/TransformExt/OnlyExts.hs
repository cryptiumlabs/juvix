-- | A transformation that discards all annotations on term/elim nodes, but
-- keeps the extensions.
module Juvix.Core.IR.TransformExt.OnlyExts
where

import Juvix.Library
import Extensible
import qualified Juvix.Core.IR.Types.Base as IR
import Juvix.Core.IR.TransformExt


data T ext

do
  ext' <- newName "ext"
  let ext = varT ext'
  decsT <- IR.extendTerm "Term" [ext'] [t|T $ext|]
    \primTy primVal -> IR.defaultExtTerm {
      IR.typeTermX = [("TermX", [[t|IR.TermX $ext $primTy $primVal|]])]
    }
  decsE <- IR.extendElim "Elim" [ext'] [t|T $ext|]
    \primTy primVal -> IR.defaultExtElim {
      IR.typeElimX = [("ElimX", [[t|IR.ElimX $ext $primTy $primVal|]])]
    }
  pure $ decsT <> decsE

onlyExtsT :: IR.Term' ext primTy primVal -> IR.Term' (T ext) primTy primVal
onlyExtsT = extTransformT transformer
onlyExtsE :: IR.Elim' ext primTy primVal -> IR.Elim' (T ext) primTy primVal
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
