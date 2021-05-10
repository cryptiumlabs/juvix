{-# LANGUAGE ViewPatterns #-}

-- | Transformations between different extensions.
module Juvix.Core.IR.TransformExt
  ( ExtTransformTEF (..),
    ExtTransformTE,
    pattern ExtTransformTE,
    -- fields of ExtTransformTE
    etStar,
    etPrimTy,
    etPrim,
    etPi,
    etLam,
    etSig,
    etPair,
    etUnitTy,
    etUnit,
    etLet,
    etElim,
    etBound,
    etFree,
    etApp,
    etAnn,
    etTermX,
    etElimX,
    extTransformTF,
    extTransformEF,
    extTransformT,
    extTransformE,
    forgetterTE,
    extForgetT,
    extForgetE,
    composeTE,
    ExtTransformVNF (..),
    ExtTransformVN,
    pattern ExtTransformVN,
    -- fields of ExtTransformVN
    etVStar,
    etVPrimTy,
    etVPrim,
    etVPi,
    etVLam,
    etVSig,
    etVPair,
    etVUnitTy,
    etVUnit,
    etVNeutral,
    etNBound,
    etNFree,
    etNApp,
    etValueX,
    etNeutralX,
    extTransformVF,
    extTransformNF,
    extTransformV,
    extTransformN,
    forgetterVN,
    extForgetV,
    extForgetN,
    composeVN,
  )
where

import Data.Coerce
import Juvix.Core.IR.Types (Elim, Neutral, NoExt, Term, Value)
import Juvix.Core.IR.Types.Base
import Juvix.Library hiding (Coerce)

data ExtTransformTEF f ext1 ext2 primTy primVal = ExtTransformTEF
  { etfStar :: XStar ext1 primTy primVal -> f (XStar ext2 primTy primVal),
    etfPrimTy :: XPrimTy ext1 primTy primVal -> f (XPrimTy ext2 primTy primVal),
    etfPrim :: XPrim ext1 primTy primVal -> f (XPrim ext2 primTy primVal),
    etfPi :: XPi ext1 primTy primVal -> f (XPi ext2 primTy primVal),
    etfLam :: XLam ext1 primTy primVal -> f (XLam ext2 primTy primVal),
    etfSig :: XSig ext1 primTy primVal -> f (XSig ext2 primTy primVal),
    etfPair :: XPair ext1 primTy primVal -> f (XPair ext2 primTy primVal),
    etfUnitTy :: XUnitTy ext1 primTy primVal -> f (XUnitTy ext2 primTy primVal),
    etfUnit :: XUnit ext1 primTy primVal -> f (XUnit ext2 primTy primVal),
    etfLet :: XLet ext1 primTy primVal -> f (XLet ext2 primTy primVal),
    etfElim :: XElim ext1 primTy primVal -> f (XElim ext2 primTy primVal),
    etfBound :: XBound ext1 primTy primVal -> f (XBound ext2 primTy primVal),
    etfFree :: XFree ext1 primTy primVal -> f (XFree ext2 primTy primVal),
    etfApp :: XApp ext1 primTy primVal -> f (XApp ext2 primTy primVal),
    etfAnn :: XAnn ext1 primTy primVal -> f (XAnn ext2 primTy primVal),
    etfTermX :: TermX ext1 primTy primVal -> f (TermX ext2 primTy primVal),
    etfElimX :: ElimX ext1 primTy primVal -> f (ElimX ext2 primTy primVal)
  }

type ExtTransformTE = ExtTransformTEF Identity

pattern Coerce :: Coercible a b => a -> b
pattern Coerce f <- (coerce -> f) where Coerce f = coerce f

pattern ExtTransformTE ::
  (XStar ext1 primTy primVal -> XStar ext2 primTy primVal) ->
  (XPrimTy ext1 primTy primVal -> XPrimTy ext2 primTy primVal) ->
  (XPrim ext1 primTy primVal -> XPrim ext2 primTy primVal) ->
  (XPi ext1 primTy primVal -> XPi ext2 primTy primVal) ->
  (XLam ext1 primTy primVal -> XLam ext2 primTy primVal) ->
  (XSig ext1 primTy primVal -> XSig ext2 primTy primVal) ->
  (XPair ext1 primTy primVal -> XPair ext2 primTy primVal) ->
  (XUnitTy ext1 primTy primVal -> XUnitTy ext2 primTy primVal) ->
  (XUnit ext1 primTy primVal -> XUnit ext2 primTy primVal) ->
  (XLet ext1 primTy primVal -> XLet ext2 primTy primVal) ->
  (XElim ext1 primTy primVal -> XElim ext2 primTy primVal) ->
  (XBound ext1 primTy primVal -> XBound ext2 primTy primVal) ->
  (XFree ext1 primTy primVal -> XFree ext2 primTy primVal) ->
  (XApp ext1 primTy primVal -> XApp ext2 primTy primVal) ->
  (XAnn ext1 primTy primVal -> XAnn ext2 primTy primVal) ->
  (TermX ext1 primTy primVal -> TermX ext2 primTy primVal) ->
  (ElimX ext1 primTy primVal -> ElimX ext2 primTy primVal) ->
  ExtTransformTE ext1 ext2 primTy primVal
pattern ExtTransformTE
  { etStar,
    etPrimTy,
    etPrim,
    etPi,
    etLam,
    etSig,
    etPair,
    etUnitTy,
    etUnit,
    etLet,
    etElim,
    etBound,
    etFree,
    etApp,
    etAnn,
    etTermX,
    etElimX
  } =
  ExtTransformTEF
    { etfStar = Coerce etStar,
      etfPrimTy = Coerce etPrimTy,
      etfPrim = Coerce etPrim,
      etfPi = Coerce etPi,
      etfLam = Coerce etLam,
      etfSig = Coerce etSig,
      etfUnitTy = Coerce etUnitTy,
      etfUnit = Coerce etUnit,
      etfPair = Coerce etPair,
      etfLet = Coerce etLet,
      etfElim = Coerce etElim,
      etfBound = Coerce etBound,
      etfFree = Coerce etFree,
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
extTransformTF fs (Prim' k e) = Prim' k <$> etfPrim fs e
extTransformTF fs (Pi' π s t e) =
  Pi' π <$> extTransformTF fs s <*> extTransformTF fs t <*> etfPi fs e
extTransformTF fs (Lam' t e) = Lam' <$> extTransformTF fs t <*> etfLam fs e
extTransformTF fs (Sig' π s t e) =
  Sig' π <$> extTransformTF fs s <*> extTransformTF fs t <*> etfSig fs e
extTransformTF fs (Pair' s t e) =
  Pair' <$> extTransformTF fs s <*> extTransformTF fs t <*> etfPair fs e
extTransformTF fs (UnitTy' e) =
  UnitTy' <$> etfUnitTy fs e
extTransformTF fs (Unit' e) =
  Unit' <$> etfUnit fs e
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
extTransformEF fs (App' f s e) =
  App' <$> extTransformEF fs f
    <*> extTransformTF fs s
    <*> etfApp fs e
extTransformEF fs (Ann' π s t ℓ e) =
  Ann' π <$> extTransformTF fs s
    <*> extTransformTF fs t
    <*> pure ℓ
    <*> etfAnn fs e
extTransformEF fs (ElimX e) = ElimX <$> etfElimX fs e

extTransformE ::
  ExtTransformTE ext1 ext2 primTy primVal ->
  Elim' ext1 primTy primVal ->
  Elim' ext2 primTy primVal
extTransformE fs t = runIdentity $ extTransformEF fs t

forgetterTE ::
  ( TermX ext primTy primVal ~ Void,
    ElimX ext primTy primVal ~ Void
  ) =>
  ExtTransformTE ext NoExt primTy primVal
forgetterTE =
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
      etTermX = absurd,
      etElimX = absurd
    }

extForgetT ::
  ( TermX ext primTy primVal ~ Void,
    ElimX ext primTy primVal ~ Void
  ) =>
  Term' ext primTy primVal ->
  Term primTy primVal
extForgetT = extTransformT forgetterTE

extForgetE ::
  ( TermX ext primTy primVal ~ Void,
    ElimX ext primTy primVal ~ Void
  ) =>
  Elim' ext primTy primVal ->
  Elim primTy primVal
extForgetE = extTransformE forgetterTE

composeTE ::
  Monad f =>
  ExtTransformTEF f ext2 ext3 primTy primVal ->
  ExtTransformTEF f ext1 ext2 primTy primVal ->
  ExtTransformTEF f ext1 ext3 primTy primVal
composeTE fs gs =
  ExtTransformTEF
    { etfStar = etfStar fs <=< etfStar gs,
      etfPrimTy = etfPrimTy fs <=< etfPrimTy gs,
      etfPrim = etfPrim fs <=< etfPrim gs,
      etfPi = etfPi fs <=< etfPi gs,
      etfSig = etfSig fs <=< etfSig gs,
      etfPair = etfPair fs <=< etfPair gs,
      etfUnitTy = etfUnitTy fs <=< etfUnitTy gs,
      etfUnit = etfUnit fs <=< etfUnit gs,
      etfLam = etfLam fs <=< etfLam gs,
      etfLet = etfLet fs <=< etfLet gs,
      etfElim = etfElim fs <=< etfElim gs,
      etfBound = etfBound fs <=< etfBound gs,
      etfFree = etfFree fs <=< etfFree gs,
      etfApp = etfApp fs <=< etfApp gs,
      etfAnn = etfAnn fs <=< etfAnn gs,
      etfTermX = etfTermX fs <=< etfTermX gs,
      etfElimX = etfElimX fs <=< etfElimX gs
    }

data ExtTransformVNF f ext1 ext2 primTy primVal = ExtTransformVNF
  { etfVStar :: XVStar ext1 primTy primVal -> f (XVStar ext2 primTy primVal),
    etfVPrimTy :: XVPrimTy ext1 primTy primVal -> f (XVPrimTy ext2 primTy primVal),
    etfVPi :: XVPi ext1 primTy primVal -> f (XVPi ext2 primTy primVal),
    etfVLam :: XVLam ext1 primTy primVal -> f (XVLam ext2 primTy primVal),
    etfVSig :: XVSig ext1 primTy primVal -> f (XVSig ext2 primTy primVal),
    etfVPair :: XVPair ext1 primTy primVal -> f (XVPair ext2 primTy primVal),
    etfVUnitTy :: XVUnitTy ext1 primTy primVal -> f (XVUnitTy ext2 primTy primVal),
    etfVUnit :: XVUnit ext1 primTy primVal -> f (XVUnit ext2 primTy primVal),
    etfVNeutral ::
      XVNeutral ext1 primTy primVal ->
      f (XVNeutral ext2 primTy primVal),
    etfVPrim :: XVPrim ext1 primTy primVal -> f (XVPrim ext2 primTy primVal),
    etfNBound :: XNBound ext1 primTy primVal -> f (XNBound ext2 primTy primVal),
    etfNFree :: XNFree ext1 primTy primVal -> f (XNFree ext2 primTy primVal),
    etfNApp :: XNApp ext1 primTy primVal -> f (XNApp ext2 primTy primVal),
    etfValueX :: ValueX ext1 primTy primVal -> f (ValueX ext2 primTy primVal),
    etfNeutralX :: NeutralX ext1 primTy primVal -> f (NeutralX ext2 primTy primVal)
  }

type ExtTransformVN = ExtTransformVNF Identity

pattern ExtTransformVN ::
  (XVStar ext1 primTy primVal -> XVStar ext2 primTy primVal) ->
  (XVPrimTy ext1 primTy primVal -> XVPrimTy ext2 primTy primVal) ->
  (XVPi ext1 primTy primVal -> XVPi ext2 primTy primVal) ->
  (XVLam ext1 primTy primVal -> XVLam ext2 primTy primVal) ->
  (XVSig ext1 primTy primVal -> XVSig ext2 primTy primVal) ->
  (XVPair ext1 primTy primVal -> XVPair ext2 primTy primVal) ->
  (XVUnitTy ext1 primTy primVal -> XVUnitTy ext2 primTy primVal) ->
  (XVUnit ext1 primTy primVal -> XVUnit ext2 primTy primVal) ->
  (XVNeutral ext1 primTy primVal -> XVNeutral ext2 primTy primVal) ->
  (XVPrim ext1 primTy primVal -> XVPrim ext2 primTy primVal) ->
  (XNBound ext1 primTy primVal -> XNBound ext2 primTy primVal) ->
  (XNFree ext1 primTy primVal -> XNFree ext2 primTy primVal) ->
  (XNApp ext1 primTy primVal -> XNApp ext2 primTy primVal) ->
  (ValueX ext1 primTy primVal -> ValueX ext2 primTy primVal) ->
  (NeutralX ext1 primTy primVal -> NeutralX ext2 primTy primVal) ->
  ExtTransformVN ext1 ext2 primTy primVal
pattern ExtTransformVN
  { etVStar,
    etVPrimTy,
    etVPi,
    etVLam,
    etVSig,
    etVPair,
    etVUnitTy,
    etVUnit,
    etVNeutral,
    etVPrim,
    etNBound,
    etNFree,
    etNApp,
    etValueX,
    etNeutralX
  } =
  ExtTransformVNF
    { etfVStar = Coerce etVStar,
      etfVPrimTy = Coerce etVPrimTy,
      etfVPi = Coerce etVPi,
      etfVLam = Coerce etVLam,
      etfVSig = Coerce etVSig,
      etfVPair = Coerce etVPair,
      etfVUnitTy = Coerce etVUnitTy,
      etfVUnit = Coerce etVUnit,
      etfVNeutral = Coerce etVNeutral,
      etfVPrim = Coerce etVPrim,
      etfNBound = Coerce etNBound,
      etfNFree = Coerce etNFree,
      etfNApp = Coerce etNApp,
      etfValueX = Coerce etValueX,
      etfNeutralX = Coerce etNeutralX
    }

extTransformVF ::
  Applicative f =>
  ExtTransformVNF f ext1 ext2 primTy primVal ->
  Value' ext1 primTy primVal ->
  f (Value' ext2 primTy primVal)
extTransformVF fs = \case
  VStar' ℓ a -> VStar' ℓ <$> etfVStar fs a
  VPrimTy' p a -> VPrimTy' p <$> etfVPrimTy fs a
  VPi' π s t a ->
    VPi' π <$> extTransformVF fs s <*> extTransformVF fs t <*> etfVPi fs a
  VLam' t a ->
    VLam' <$> extTransformVF fs t <*> etfVLam fs a
  VSig' π s t a ->
    VSig' π <$> extTransformVF fs s <*> extTransformVF fs t <*> etfVSig fs a
  VPair' s t a ->
    VPair' <$> extTransformVF fs s <*> extTransformVF fs t <*> etfVPair fs a
  VUnitTy' a -> VUnitTy' <$> etfVUnitTy fs a
  VUnit' a -> VUnit' <$> etfVUnit fs a
  VNeutral' n a -> VNeutral' <$> extTransformNF fs n <*> etfVNeutral fs a
  VPrim' p a -> VPrim' p <$> etfVPrim fs a
  ValueX a -> ValueX <$> etfValueX fs a

extTransformV ::
  ExtTransformVN ext1 ext2 primTy primVal ->
  Value' ext1 primTy primVal ->
  Value' ext2 primTy primVal
extTransformV fs = runIdentity . extTransformVF fs

extTransformNF ::
  Applicative f =>
  ExtTransformVNF f ext1 ext2 primTy primVal ->
  Neutral' ext1 primTy primVal ->
  f (Neutral' ext2 primTy primVal)
extTransformNF fs = \case
  NBound' i a -> NBound' i <$> etfNBound fs a
  NFree' x a -> NFree' x <$> etfNFree fs a
  NApp' f s a ->
    NApp' <$> extTransformNF fs f <*> extTransformVF fs s <*> etfNApp fs a
  NeutralX a -> NeutralX <$> etfNeutralX fs a

extTransformN ::
  ExtTransformVN ext1 ext2 primTy primVal ->
  Neutral' ext1 primTy primVal ->
  Neutral' ext2 primTy primVal
extTransformN fs = runIdentity . extTransformNF fs

forgetterVN ::
  ( ValueX ext primTy primVal ~ Void,
    NeutralX ext primTy primVal ~ Void
  ) =>
  ExtTransformVN ext NoExt primTy primVal
forgetterVN =
  ExtTransformVN
    { etVStar = const (),
      etVPrimTy = const (),
      etVPrim = const (),
      etVPi = const (),
      etVSig = const (),
      etVPair = const (),
      etVUnitTy = const (),
      etVUnit = const (),
      etVLam = const (),
      etVNeutral = const (),
      etNBound = const (),
      etNFree = const (),
      etNApp = const (),
      etValueX = absurd,
      etNeutralX = absurd
    }

extForgetV ::
  (ValueX ext primTy primVal ~ Void, NeutralX ext primTy primVal ~ Void) =>
  Value' ext primTy primVal ->
  Value primTy primVal
extForgetV = extTransformV forgetterVN

extForgetN ::
  (ValueX ext primTy primVal ~ Void, NeutralX ext primTy primVal ~ Void) =>
  Neutral' ext primTy primVal ->
  Neutral primTy primVal
extForgetN = extTransformN forgetterVN

composeVN ::
  Monad f =>
  ExtTransformVNF f ext2 ext3 primTy primVal ->
  ExtTransformVNF f ext1 ext2 primTy primVal ->
  ExtTransformVNF f ext1 ext3 primTy primVal
composeVN fs gs =
  ExtTransformVNF
    { etfVStar = etfVStar fs <=< etfVStar gs,
      etfVPrimTy = etfVPrimTy fs <=< etfVPrimTy gs,
      etfVPi = etfVPi fs <=< etfVPi gs,
      etfVLam = etfVLam fs <=< etfVLam gs,
      etfVSig = etfVSig fs <=< etfVSig gs,
      etfVPair = etfVPair fs <=< etfVPair gs,
      etfVUnitTy = etfVUnitTy fs <=< etfVUnitTy gs,
      etfVUnit = etfVUnit fs <=< etfVUnit gs,
      etfVNeutral = etfVNeutral fs <=< etfVNeutral gs,
      etfVPrim = etfVPrim fs <=< etfVPrim gs,
      etfNBound = etfNBound fs <=< etfNBound gs,
      etfNFree = etfNFree fs <=< etfNFree gs,
      etfNApp = etfNApp fs <=< etfNApp gs,
      etfValueX = etfValueX fs <=< etfValueX gs,
      etfNeutralX = etfNeutralX fs <=< etfNeutralX gs
    }
