{-# LANGUAGE ViewPatterns #-}

-- | Transformations between different extensions.
module Juvix.Core.IR.TransformExt where

import Data.Coerce
import Juvix.Core.IR.Types (Elim, NoExt, Term)
import Juvix.Core.IR.Types.Base
import Juvix.Library hiding (Coerce)

data ExtTransformTEF f ext1 ext2 primTy primVal
  = ExtTransformTEF
      { etfStar :: XStar ext1 primTy primVal -> f (XStar ext2 primTy primVal),
        etfPrimTy :: XPrimTy ext1 primTy primVal -> f (XPrimTy ext2 primTy primVal),
        etfPi :: XPi ext1 primTy primVal -> f (XPi ext2 primTy primVal),
        etfLam :: XLam ext1 primTy primVal -> f (XLam ext2 primTy primVal),
        etfLet :: XLet ext1 primTy primVal -> f (XLet ext2 primTy primVal),
        etfElim :: XElim ext1 primTy primVal -> f (XElim ext2 primTy primVal),
        etfBound :: XBound ext1 primTy primVal -> f (XBound ext2 primTy primVal),
        etfFree :: XFree ext1 primTy primVal -> f (XFree ext2 primTy primVal),
        etfPrim :: XPrim ext1 primTy primVal -> f (XPrim ext2 primTy primVal),
        etfApp :: XApp ext1 primTy primVal -> f (XApp ext2 primTy primVal),
        etfAnn :: XAnn ext1 primTy primVal -> f (XAnn ext2 primTy primVal),
        etfTermX :: TermX ext1 primTy primVal -> f (TermX ext2 primTy primVal),
        etfElimX :: ElimX ext1 primTy primVal -> f (ElimX ext2 primTy primVal)
      }

type ExtTransformTE = ExtTransformTEF Identity

pattern Coerce :: Coercible a b => a -> b
pattern Coerce f <-
  (coerce -> f)
  where
    Coerce f = coerce f

pattern ExtTransformTE ::
  (XStar ext1 primTy primVal -> XStar ext2 primTy primVal) ->
  (XPrimTy ext1 primTy primVal -> XPrimTy ext2 primTy primVal) ->
  (XPi ext1 primTy primVal -> XPi ext2 primTy primVal) ->
  (XLam ext1 primTy primVal -> XLam ext2 primTy primVal) ->
  (XLet ext1 primTy primVal -> XLet ext2 primTy primVal) ->
  (XElim ext1 primTy primVal -> XElim ext2 primTy primVal) ->
  (XBound ext1 primTy primVal -> XBound ext2 primTy primVal) ->
  (XFree ext1 primTy primVal -> XFree ext2 primTy primVal) ->
  (XPrim ext1 primTy primVal -> XPrim ext2 primTy primVal) ->
  (XApp ext1 primTy primVal -> XApp ext2 primTy primVal) ->
  (XAnn ext1 primTy primVal -> XAnn ext2 primTy primVal) ->
  (TermX ext1 primTy primVal -> TermX ext2 primTy primVal) ->
  (ElimX ext1 primTy primVal -> ElimX ext2 primTy primVal) ->
  ExtTransformTE ext1 ext2 primTy primVal
pattern ExtTransformTE
  { etStar,
    etPrimTy,
    etPi,
    etLam,
    etLet,
    etElim,
    etBound,
    etFree,
    etPrim,
    etApp,
    etAnn,
    etTermX,
    etElimX
  } =
  ExtTransformTEF
    { etfStar = Coerce etStar,
      etfPrimTy = Coerce etPrimTy,
      etfPi = Coerce etPi,
      etfLam = Coerce etLam,
      etfLet = Coerce etLet,
      etfElim = Coerce etElim,
      etfBound = Coerce etBound,
      etfFree = Coerce etFree,
      etfPrim = Coerce etPrim,
      etfApp = Coerce etApp,
      etfAnn = Coerce etAnn,
      etfTermX = Coerce etTermX,
      etfElimX = Coerce etElimX
    }

extTransformTF ::
  Applicative f =>
  ExtTransformTEF f ext1 ext2 primTy primVal ->
  Term' ext1 primTy primVal ->
  f (Term' ext2 primTy primVal)
extTransformTF fs (Star' i e) = Star' i <$> etfStar fs e
extTransformTF fs (PrimTy' k e) = PrimTy' k <$> etfPrimTy fs e
extTransformTF fs (Pi' π s t e) =
  Pi' π <$> extTransformTF fs s <*> extTransformTF fs t <*> etfPi fs e
extTransformTF fs (Lam' t e) = Lam' <$> extTransformTF fs t <*> etfLam fs e
extTransformTF fs (Let' π l b e) =
  Let' π <$> extTransformEF fs l <*> extTransformTF fs b <*> etfLet fs e
extTransformTF fs (Elim' f e) = Elim' <$> extTransformEF fs f <*> etfElim fs e
extTransformTF fs (TermX e) = TermX <$> etfTermX fs e

extTransformT ::
  ExtTransformTE ext1 ext2 primTy primVal ->
  Term' ext1 primTy primVal ->
  Term' ext2 primTy primVal
extTransformT fs t = runIdentity $ extTransformTF fs t

extTransformEF ::
  Applicative f =>
  ExtTransformTEF f ext1 ext2 primTy primVal ->
  Elim' ext1 primTy primVal ->
  f (Elim' ext2 primTy primVal)
extTransformEF fs (Bound' x e) = Bound' x <$> etfBound fs e
extTransformEF fs (Free' x e) = Free' x <$> etfFree fs e
extTransformEF fs (Prim' k e) = Prim' k <$> etfPrim fs e
extTransformEF fs (App' f s e) =
  App' <$> extTransformEF fs f
    <*> extTransformTF fs s
    <*> etfApp fs e
extTransformEF fs (Ann' π s t l e) =
  Ann' π <$> extTransformTF fs s
    <*> extTransformTF fs t
    <*> pure l
    <*> etfAnn fs e
extTransformEF fs (ElimX e) = ElimX <$> etfElimX fs e

extTransformE ::
  ExtTransformTE ext1 ext2 primTy primVal ->
  Elim' ext1 primTy primVal ->
  Elim' ext2 primTy primVal
extTransformE fs t = runIdentity $ extTransformEF fs t

forgetter ::
  ( TermX ext primTy primVal ~ Void,
    ElimX ext primTy primVal ~ Void
  ) =>
  ExtTransformTE ext NoExt primTy primVal
forgetter =
  ExtTransformTE
    { etStar = const (),
      etPrimTy = const (),
      etPi = const (),
      etLam = const (),
      etLet = const (),
      etElim = const (),
      etBound = const (),
      etFree = const (),
      etPrim = const (),
      etApp = const (),
      etAnn = const (),
      etTermX = absurd,
      etElimX = absurd
    }

extForgetT ::
  ( TermX ext primTy primVal ~ Void,
    ElimX ext primTy primVal ~ Void
  ) =>
  Term' ext primTy primVal ->
  Term primTy primVal
extForgetT = extTransformT forgetter

extForgetE ::
  ( TermX ext primTy primVal ~ Void,
    ElimX ext primTy primVal ~ Void
  ) =>
  Elim' ext primTy primVal ->
  Elim primTy primVal
extForgetE = extTransformE forgetter
