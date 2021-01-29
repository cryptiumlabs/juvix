{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- This includes the evaluators (evalTerm and evalElim),
-- the value application function (vapp) and
-- the substitution functions (substTerm and substElim).
module Juvix.Core.IR.Evaluator where

import Data.Foldable (foldr1) -- on NonEmpty
import qualified Data.IntMap as IntMap
import qualified Juvix.Core.Application as App
import Juvix.Core.IR.TransformExt
import qualified Juvix.Core.IR.TransformExt.OnlyExts as OnlyExts
import qualified Juvix.Core.IR.Types as IR
import qualified Juvix.Core.IR.Types.Base as IR
import qualified Juvix.Core.Parameterisation as Param
import Juvix.Library
import qualified Juvix.Library.Usage as Usage

inlineAllGlobals ::
  ( EvalPatSubst ext' primTy primVal,
    IR.ToTerm ty IR.NoExt IR.NoExt,
    NoExtensions ext primTy primVal
  ) =>
  IR.Term' (OnlyExts.T ext') primTy primVal ->
  (IR.Name -> Maybe (IR.GlobalWith (ty IR.NoExt) ext primTy primVal)) ->
  IR.Term' (OnlyExts.T ext') primTy primVal
inlineAllGlobals t map =
  case t of
    IR.Unit -> t
    IR.UnitTy -> t
    IR.Pair p1 p2 ->
      IR.Pair (inlineAllGlobals p1 map) (inlineAllGlobals p2 map)
    IR.Elim elim ->
      IR.Elim (inlineAllGlobalsElim elim map)
    IR.Sig u t1 t2 ->
      IR.Sig u (inlineAllGlobals t1 map) (inlineAllGlobals t2 map)
    IR.Let u e t ->
      IR.Let u (inlineAllGlobalsElim e map) (inlineAllGlobals t map)
    IR.Lam t ->
      IR.Lam (inlineAllGlobals t map)
    IR.Pi u t1 t2 ->
      IR.Pi u (inlineAllGlobals t1 map) (inlineAllGlobals t2 map)
    IR.Prim {} -> t
    IR.PrimTy {} -> t
    IR.Star {} -> t

inlineAllGlobalsElim ::
  ( EvalPatSubst ext' primTy primVal,
    IR.ToTerm ty IR.NoExt IR.NoExt,
    NoExtensions ext primTy primVal
  ) =>
  IR.Elim' (OnlyExts.T ext') primTy primVal ->
  (IR.Name -> Maybe (IR.GlobalWith (ty IR.NoExt) ext primTy primVal)) ->
  IR.Elim' (OnlyExts.T ext') primTy primVal
inlineAllGlobalsElim t map =
  case t of
    IR.Bound {} -> t
    IR.Free name -> fromMaybe t $ map name >>= toLambda
    IR.App elim term ->
      IR.App (inlineAllGlobalsElim elim map) (inlineAllGlobals term map)
    IR.Ann u t1 t2 uni ->
      IR.Ann u (inlineAllGlobals t1 map) (inlineAllGlobals t2 map) uni

class HasWeak a where
  weakBy' :: Natural -> IR.BoundVar -> a -> a
  default weakBy' ::
    (Generic a, GHasWeak (Rep a)) =>
    Natural ->
    IR.BoundVar ->
    a ->
    a
  weakBy' b i = to . gweakBy' b i . from

weakBy :: HasWeak a => Natural -> a -> a
weakBy b = weakBy' b 0

weak' :: HasWeak a => IR.BoundVar -> a -> a
weak' = weakBy' 1

weak :: HasWeak a => a -> a
weak = weak' 0

type AllWeak ext primTy primVal =
  ( HasWeak primTy,
    HasWeak primVal,
    IR.TermAll HasWeak ext primTy primVal,
    IR.ElimAll HasWeak ext primTy primVal
  )

instance AllWeak ext primTy primVal => HasWeak (IR.Term' ext primTy primVal) where
  weakBy' b i (IR.Star' u a) =
    IR.Star' u (weakBy' b i a)
  weakBy' b i (IR.PrimTy' p a) =
    IR.PrimTy' (weakBy' b i p) (weakBy' b i a)
  weakBy' b i (IR.Prim' p a) =
    IR.Prim' (weakBy' b i p) (weakBy' b i a)
  weakBy' b i (IR.Pi' π s t a) =
    IR.Pi' π (weakBy' b i s) (weakBy' b (succ i) t) (weakBy' b i a)
  weakBy' b i (IR.Lam' t a) =
    IR.Lam' (weakBy' b (succ i) t) (weakBy' b i a)
  weakBy' b i (IR.Sig' π s t a) =
    IR.Sig' π (weakBy' b i s) (weakBy' b (succ i) t) (weakBy' b i a)
  weakBy' b i (IR.Pair' s t a) =
    IR.Pair' (weakBy' b i s) (weakBy' b i t) (weakBy' b i a)
  weakBy' b i (IR.UnitTy' a) =
    IR.UnitTy' (weakBy' b i a)
  weakBy' b i (IR.Unit' a) =
    IR.Unit' (weakBy' b i a)
  weakBy' b i (IR.Let' π s t a) =
    IR.Let' π (weakBy' b i s) (weakBy' b (succ i) t) (weakBy' b i a)
  weakBy' b i (IR.Elim' f a) =
    IR.Elim' (weakBy' b i f) (weakBy' b i a)
  weakBy' b i (IR.TermX a) =
    IR.TermX (weakBy' b i a)

instance AllWeak ext primTy primVal => HasWeak (IR.Elim' ext primTy primVal) where
  weakBy' b i (IR.Bound' j a)
    | j >= i = IR.Bound' (j + b) a'
    | otherwise = IR.Bound' j a'
    where
      a' = weakBy' b i a
  weakBy' b i (IR.Free' x a) =
    IR.Free' x (weakBy' b i a)
  weakBy' b i (IR.App' s t a) =
    IR.App' (weakBy' b i s) (weakBy' b i t) (weakBy' b i a)
  weakBy' b i (IR.Ann' π s t l a) =
    IR.Ann' π (weakBy' b i s) (weakBy' b i t) l (weakBy' b i a)
  weakBy' b i (IR.ElimX a) =
    IR.ElimX (weakBy' b i a)

class HasWeak a => HasSubst ext primTy primVal a where
  substWith ::
    -- | How many bindings have been traversed so far
    IR.BoundVar ->
    -- | Variable to substitute
    IR.BoundVar ->
    -- | Expression to substitute with
    IR.Elim' ext primTy primVal ->
    a ->
    a
  default substWith ::
    (Generic a, GHasSubst ext primTy primVal (Rep a)) =>
    Natural ->
    IR.BoundVar ->
    IR.Elim' ext primTy primVal ->
    a ->
    a
  substWith b i e = to . gsubstWith b i e . from

subst' ::
  HasSubst ext primTy primVal a =>
  IR.BoundVar ->
  IR.Elim' ext primTy primVal ->
  a ->
  a
subst' = substWith 0

subst ::
  HasSubst ext primTy primVal a =>
  IR.Elim' ext primTy primVal ->
  a ->
  a
subst = subst' 0

class HasWeak a => HasSubstTerm ext primTy primVal a where
  substTermWith ::
    -- | How many bindings have been traversed so far
    IR.BoundVar ->
    -- | Variable to substitute
    IR.BoundVar ->
    -- | Expression to substitute with
    IR.Elim' ext primTy primVal ->
    a ->
    IR.Term' ext primTy primVal

substTerm' ::
  HasSubstTerm ext primTy primVal a =>
  IR.BoundVar ->
  IR.Elim' ext primTy primVal ->
  a ->
  IR.Term' ext primTy primVal
substTerm' = substTermWith 0

substTerm ::
  HasSubstTerm ext primTy primVal a =>
  IR.Elim' ext primTy primVal ->
  a ->
  IR.Term' ext primTy primVal
substTerm = substTerm' 0

type AllSubst ext primTy primVal =
  ( IR.TermAll (HasSubst ext primTy primVal) ext primTy primVal,
    IR.ElimAll (HasSubst ext primTy primVal) ext primTy primVal,
    HasSubstTerm ext primTy primVal primTy,
    HasSubstTerm ext primTy primVal primVal
  )

instance
  AllSubst ext primTy primVal =>
  HasSubst ext primTy primVal (IR.Term' ext primTy primVal)
  where
  substWith w i e (IR.Star' u a) =
    IR.Star' u (substWith w i e a)
  substWith w i e (IR.PrimTy' t _) =
    -- FIXME annotation?
    substTermWith w i e t
  substWith w i e (IR.Prim' p _) =
    -- FIXME annotation?
    substTermWith w i e p
  substWith w i e (IR.Pi' π s t a) =
    IR.Pi' π (substWith w i e s) (substWith (succ w) (succ i) e t) (substWith w i e a)
  substWith w i e (IR.Lam' t a) =
    IR.Lam' (substWith (succ w) (succ i) e t) (substWith w i e a)
  substWith w i e (IR.Sig' π s t a) =
    IR.Sig' π (substWith w i e s) (substWith (succ w) (succ i) e t) (substWith w i e a)
  substWith w i e (IR.Pair' s t a) =
    IR.Pair' (substWith w i e s) (substWith w i e t) (substWith w i e a)
  substWith w i e (IR.UnitTy' a) =
    IR.UnitTy' (substWith w i e a)
  substWith w i e (IR.Unit' a) =
    IR.Unit' (substWith w i e a)
  substWith w i e (IR.Let' π l b a) =
    IR.Let' π (substWith w i e l) (substWith (succ w) (succ i) e b) (substWith w i e a)
  substWith w i e (IR.Elim' t a) =
    IR.Elim' (substWith w i e t) (substWith w i e a)
  substWith w i e (IR.TermX a) =
    IR.TermX (substWith w i e a)

instance
  AllSubst ext primTy primVal =>
  HasSubst ext primTy primVal (IR.Elim' ext primTy primVal)
  where
  substWith w i e (IR.Bound' j a) =
    case compare j i of
      LT -> IR.Bound' j a'
      EQ -> weakBy w e
      GT -> IR.Bound' (pred j) a'
    where
      a' = substWith w i e a
  substWith w i e (IR.Free' x a) =
    IR.Free' x (substWith w i e a)
  substWith w i e (IR.App' f s a) =
    IR.App' (substWith w i e f) (substWith w i e s) (substWith w i e a)
  substWith w i e (IR.Ann' π s t l a) =
    IR.Ann' π (substWith w i e s) (substWith w i e t) l (substWith w i e a)
  substWith w i e (IR.ElimX a) =
    IR.ElimX (substWith w i e a)

class HasWeak a => HasPatSubst extT primTy primVal a where
  -- returns either a substituted term or an unbound pattern var
  -- TODO: use @validation@ to return all unbound vars
  patSubst' ::
    -- | How many bindings have been traversed so far
    Natural ->
    -- | Mapping of pattern variables to matched subterms
    IR.PatternMap (IR.Elim' extT primTy primVal) ->
    a ->
    Either IR.PatternVar a
  default patSubst' ::
    ( Generic a,
      GHasPatSubst extT primTy primVal (Rep a)
    ) =>
    Natural ->
    IR.PatternMap (IR.Elim' extT primTy primVal) ->
    a ->
    Either IR.PatternVar a
  patSubst' b m = fmap to . gpatSubst' b m . from

patSubst ::
  (HasPatSubst extT primTy primVal a) =>
  IR.PatternMap (IR.Elim' extT primTy primVal) ->
  a ->
  Either IR.PatternVar a
patSubst = patSubst' 0

class HasWeak a => HasPatSubstTerm extT primTy primVal a where
  -- returns either a substituted term or an unbound pattern var
  -- TODO: use @validation@ to return all unbound vars
  patSubstTerm' ::
    -- | How many bindings have been traversed so far
    Natural ->
    -- | Mapping of pattern variables to matched subterms
    IR.PatternMap (IR.Elim' extT primTy primVal) ->
    a ->
    Either IR.PatternVar (IR.Term' extT primTy primVal)

patSubstTerm ::
  (HasPatSubstTerm extT primTy primVal a) =>
  IR.PatternMap (IR.Elim' extT primTy primVal) ->
  a ->
  Either IR.PatternVar (IR.Term' extT primTy primVal)
patSubstTerm = patSubstTerm' 0

type AllPatSubst ext primTy primVal =
  ( IR.TermAll (HasPatSubst ext primTy primVal) ext primTy primVal,
    IR.ElimAll (HasPatSubst ext primTy primVal) ext primTy primVal,
    HasPatSubstTerm ext primTy primVal primTy,
    HasPatSubstTerm ext primTy primVal primVal
  )

instance
  AllPatSubst ext primTy primVal =>
  HasPatSubst ext primTy primVal (IR.Term' ext primTy primVal)
  where
  patSubst' b m (IR.Star' u a) =
    IR.Star' u <$> patSubst' b m a
  patSubst' b m (IR.PrimTy' t _) =
    -- FIXME annotation?
    patSubstTerm' b m t
  patSubst' b m (IR.Prim' p _) =
    -- FIXME annotation?
    patSubstTerm' b m p
  patSubst' b m (IR.Pi' π s t a) =
    IR.Pi' π <$> patSubst' b m s
      <*> patSubst' (succ b) m t
      <*> patSubst' b m a
  patSubst' b m (IR.Lam' t a) =
    IR.Lam' <$> patSubst' (succ b) m t
      <*> patSubst' b m a
  patSubst' b m (IR.Sig' π s t a) =
    IR.Sig' π <$> patSubst' b m s
      <*> patSubst' (succ b) m t
      <*> patSubst' b m a
  patSubst' b m (IR.Pair' s t a) =
    IR.Pair' <$> patSubst' b m s
      <*> patSubst' b m t
      <*> patSubst' b m a
  patSubst' b m (IR.UnitTy' a) =
    IR.UnitTy' <$> patSubst' b m a
  patSubst' b m (IR.Unit' a) =
    IR.Unit' <$> patSubst' b m a
  patSubst' b m (IR.Let' π l t a) =
    IR.Let' π <$> patSubst' b m l
      <*> patSubst' (succ b) m t
      <*> patSubst' b m a
  patSubst' b m (IR.Elim' e a) =
    IR.Elim' <$> patSubst' b m e
      <*> patSubst' b m a
  patSubst' b m (IR.TermX a) =
    IR.TermX <$> patSubst' b m a

instance
  AllPatSubst ext primTy primVal =>
  HasPatSubst ext primTy primVal (IR.Elim' ext primTy primVal)
  where
  patSubst' b m (IR.Bound' j a) =
    IR.Bound' j <$> patSubst' b m a
  patSubst' b m (IR.Free' (IR.Pattern x) _) =
    case IntMap.lookup x m of
      Nothing -> Left x
      Just e -> pure $ weakBy b e
  patSubst' b m (IR.Free' x a) =
    IR.Free' x <$> patSubst' b m a
  patSubst' b m (IR.App' f e a) =
    IR.App' <$> patSubst' b m f
      <*> patSubst' b m e
      <*> patSubst' b m a
  patSubst' b m (IR.Ann' π s t ℓ a) =
    IR.Ann' π <$> patSubst' b m s
      <*> patSubst' b m t
      <*> pure ℓ
      <*> patSubst' b m a
  patSubst' b m (IR.ElimX a) =
    IR.ElimX <$> patSubst' b m a

type AllWeakV ext primTy primVal =
  ( HasWeak primTy,
    HasWeak primVal,
    IR.ValueAll HasWeak ext primTy primVal,
    IR.NeutralAll HasWeak ext primTy primVal
  )

instance
  AllWeakV ext primTy primVal =>
  HasWeak (IR.Value' ext primTy primVal)
  where
  weakBy' b i (IR.VStar' n a) =
    IR.VStar' n (weakBy' b i a)
  weakBy' b i (IR.VPrimTy' p a) =
    IR.VPrimTy' (weakBy' b i p) (weakBy' b i a)
  weakBy' b i (IR.VPi' π s t a) =
    IR.VPi' π (weakBy' b i s) (weakBy' b (succ i) t) (weakBy' b i a)
  weakBy' b i (IR.VLam' t a) =
    IR.VLam' (weakBy' b (succ i) t) (weakBy' b i a)
  weakBy' b i (IR.VSig' π s t a) =
    IR.VSig' π (weakBy' b i s) (weakBy' b (succ i) t) (weakBy' b i a)
  weakBy' b i (IR.VPair' s t a) =
    IR.VPair' (weakBy' b i s) (weakBy' b (succ i) t) (weakBy' b i a)
  weakBy' b i (IR.VUnitTy' a) =
    IR.VUnitTy' (weakBy' b i a)
  weakBy' b i (IR.VUnit' a) =
    IR.VUnit' (weakBy' b i a)
  weakBy' b i (IR.VNeutral' n a) =
    IR.VNeutral' (weakBy' b i n) (weakBy' b i a)
  weakBy' b i (IR.VPrim' p a) =
    IR.VPrim' (weakBy' b i p) (weakBy' b i a)
  weakBy' b i (IR.ValueX a) =
    IR.ValueX (weakBy' b i a)

instance
  AllWeakV ext primTy primVal =>
  HasWeak (IR.Neutral' ext primTy primVal)
  where
  weakBy' b i (IR.NBound' j a)
    | j >= i = IR.NBound' (j + b) a'
    | otherwise = IR.NBound' j a'
    where
      a' = weakBy' b i a
  weakBy' b i (IR.NFree' x a) =
    IR.NFree' x (weakBy' b i a)
  weakBy' b i (IR.NApp' f s a) =
    IR.NApp' (weakBy' b i f) (weakBy' b i s) (weakBy' b i a)
  weakBy' b i (IR.NeutralX a) =
    IR.NeutralX (weakBy' b i a)

class HasWeak a => HasSubstV extV primTy primVal a where
  substVWith ::
    Natural ->
    IR.BoundVar ->
    IR.Value' extV primTy primVal ->
    a ->
    Either (Error extV extT primTy primVal) a
  default substVWith ::
    ( Generic a,
      GHasSubstV extV primTy primVal (Rep a)
    ) =>
    Natural ->
    IR.BoundVar ->
    IR.Value' extV primTy primVal ->
    a ->
    Either (Error extV extT primTy primVal) a
  substVWith b i e = fmap to . gsubstVWith b i e . from

substV' ::
  HasSubstV extV primTy primVal a =>
  IR.BoundVar ->
  IR.Value' extV primTy primVal ->
  a ->
  Either (Error extV extT primTy primVal) a
substV' = substVWith 0

substV ::
  HasSubstV extV primTy primVal a =>
  IR.Value' extV primTy primVal ->
  a ->
  Either (Error extV extT primTy primVal) a
substV = substV' 0

class HasWeak a => HasSubstValue extV primTy primVal a where
  substValueWith ::
    Natural ->
    IR.BoundVar ->
    IR.Value' extV primTy primVal ->
    a ->
    Either (Error extV extT primTy primVal) (IR.Value' extV primTy primVal)

substValue' ::
  HasSubstValue extV primTy primVal a =>
  IR.BoundVar ->
  IR.Value' extV primTy primVal ->
  a ->
  Either (Error extV extT primTy primVal) (IR.Value' extV primTy primVal)
substValue' = substValueWith 0

substValue ::
  HasSubstValue extV primTy primVal a =>
  IR.Value' extV primTy primVal ->
  a ->
  Either (Error extV extT primTy primVal) (IR.Value' extV primTy primVal)
substValue = substValue' 0

type AllSubstV extV primTy primVal =
  ( IR.ValueAll (HasSubstV extV primTy primVal) extV primTy primVal,
    IR.NeutralAll (HasSubstV extV primTy primVal) extV primTy primVal,
    HasSubstValue extV primTy primVal primTy,
    HasSubstValue extV primTy primVal primVal,
    Param.CanApply primTy,
    Param.CanApply primVal
  )

instance
  ( AllSubstV extV primTy primVal,
    Monoid (IR.XVNeutral extV primTy primVal),
    Monoid (IR.XVLam extV primTy primVal),
    Monoid (IR.XVPrimTy extV primTy primVal),
    Monoid (IR.XVPrim extV primTy primVal)
  ) =>
  HasSubstV extV primTy primVal (IR.Value' extV primTy primVal)
  where
  substVWith w i e (IR.VStar' n a) =
    IR.VStar' n <$> substVWith w i e a
  substVWith w i e (IR.VPrimTy' p _) =
    -- TODO what about the annotation?
    substValueWith w i e p
  substVWith w i e (IR.VPi' π s t a) =
    IR.VPi' π <$> substVWith w i e s
      <*> substVWith (succ w) (succ i) e t
      <*> substVWith w i e a
  substVWith w i e (IR.VLam' t a) =
    IR.VLam' <$> substVWith (succ w) (succ i) e t
      <*> substVWith w i e a
  substVWith w i e (IR.VSig' π s t a) =
    IR.VSig' π <$> substVWith w i e s
      <*> substVWith (succ w) (succ i) e t
      <*> substVWith w i e a
  substVWith w i e (IR.VPair' s t a) =
    IR.VPair' <$> substVWith w i e s
      <*> substVWith w i e t
      <*> substVWith w i e a
  substVWith w i e (IR.VUnitTy' a) =
    IR.VUnitTy' <$> substVWith w i e a
  substVWith w i e (IR.VUnit' a) =
    IR.VUnit' <$> substVWith w i e a
  substVWith w i e (IR.VNeutral' n a) =
    substNeutralWith w i e n a
  substVWith w i e (IR.VPrim' p _) =
    -- TODO what about the annotation?
    substValueWith w i e p
  substVWith w i e (IR.ValueX a) =
    IR.ValueX <$> substVWith w i e a

-- (not quite an instance of @HasSubstValue@ because of the @XVNeutral@ stuff)
substNeutralWith ::
  ( AllSubstV extV primTy primVal,
    Monoid (IR.XVNeutral extV primTy primVal),
    Monoid (IR.XVLam extV primTy primVal),
    Monoid (IR.XVPrimTy extV primTy primVal),
    Monoid (IR.XVPrim extV primTy primVal)
  ) =>
  Natural ->
  IR.BoundVar ->
  IR.Value' extV primTy primVal ->
  IR.Neutral' extV primTy primVal ->
  IR.XVNeutral extV primTy primVal ->
  Either (Error extV extT primTy primVal) (IR.Value' extV primTy primVal)
-- not Neutral'!!!
substNeutralWith w i e (IR.NBound' j a) b = do
  a' <- substVWith w i e a
  b' <- substVWith w i e b
  pure $ case compare j i of
    LT -> IR.VNeutral' (IR.NBound' j a') b'
    EQ -> weakBy w e
    GT -> IR.VNeutral' (IR.NBound' (pred j) a') b'
substNeutralWith w i e (IR.NFree' x a) b =
  IR.VNeutral' <$> (IR.NFree' x <$> substVWith w i e a)
    <*> substVWith w i e b
substNeutralWith w i e (IR.NApp' f s a) _ =
  join $
    vapp <$> substNeutralWith w i e f mempty
      <*> substVWith w i e s
      <*> substVWith w i e a
substNeutralWith w i e (IR.NeutralX a) b =
  IR.VNeutral' <$> (IR.NeutralX <$> substVWith w i e a)
    <*> substVWith w i e b

data ApplyError primTy primVal
  = NoApplyError
  | ApplyErrorV (Param.ApplyError primVal)
  | ApplyErrorT (Param.ApplyError primTy)

deriving instance
  ( Eq primTy,
    Eq primVal,
    Eq (Param.ApplyErrorExtra primTy),
    Eq (Param.ApplyErrorExtra primVal)
  ) =>
  Eq (ApplyError primTy primVal)

deriving instance
  ( Show primTy,
    Show primVal,
    Show (Param.ApplyErrorExtra primTy),
    Show (Param.ApplyErrorExtra primVal)
  ) =>
  Show (ApplyError primTy primVal)

data Error extV extT primTy primVal
  = CannotApply
      { fun, arg :: IR.Value' extV primTy primVal,
        paramErr :: ApplyError primTy primVal
      }
  | UnsupportedTermExt (IR.TermX extT primTy primVal)
  | UnsupportedElimExt (IR.ElimX extT primTy primVal)

deriving instance
  ( Eq primTy,
    Eq primVal,
    IR.ValueAll Eq extV primTy primVal,
    IR.NeutralAll Eq extV primTy primVal,
    Eq (Param.ApplyErrorExtra primTy),
    Eq (Param.ApplyErrorExtra primVal),
    Eq (IR.TermX extT primTy primVal),
    Eq (IR.ElimX extT primTy primVal)
  ) =>
  Eq (Error extV extT primTy primVal)

deriving instance
  ( Show primTy,
    Show primVal,
    IR.ValueAll Show extV primTy primVal,
    IR.NeutralAll Show extV primTy primVal,
    Show (Param.ApplyErrorExtra primTy),
    Show (Param.ApplyErrorExtra primVal),
    Show (IR.TermX extT primTy primVal),
    Show (IR.ElimX extT primTy primVal)
  ) =>
  Show (Error extV extT primTy primVal)

vapp ::
  ( AllSubstV extV primTy primVal,
    Monoid (IR.XVNeutral extV primTy primVal),
    Monoid (IR.XVLam extV primTy primVal),
    Monoid (IR.XVPrimTy extV primTy primVal),
    Monoid (IR.XVPrim extV primTy primVal)
  ) =>
  IR.Value' extV primTy primVal ->
  IR.Value' extV primTy primVal ->
  -- | the annotation to use if the result is another application node
  -- (if it isn't, then this annotation is unused)
  IR.XNApp extV primTy primVal ->
  Either (Error extV extT primTy primVal) (IR.Value' extV primTy primVal)
vapp (IR.VLam' t _) s _ =
  substV s t
vapp (IR.VNeutral' f _) s b =
  pure $ IR.VNeutral' (IR.NApp' f s b) mempty
vapp pp@(IR.VPrimTy' p _) qq@(IR.VPrimTy' q _) _ =
  bimap (CannotApply pp qq . ApplyErrorT) (\pq -> IR.VPrimTy' pq mempty) $
    Param.apply1 p q
vapp pp@(IR.VPrim' p _) qq@(IR.VPrim' q _) _ =
  bimap (CannotApply pp qq . ApplyErrorV) (\pq -> IR.VPrim' pq mempty) $
    Param.apply1 p q
vapp f x _ =
  Left $ CannotApply f x NoApplyError

type TermExtFun ty ext' ext primTy primVal =
  LookupFun ty ext' primTy primVal ->
  IR.TermX ext primTy primVal ->
  Either (Error IR.NoExt ext primTy primVal) (IR.Value primTy primVal)

type ElimExtFun ty ext' ext primTy primVal =
  LookupFun ty ext' primTy primVal ->
  IR.ElimX ext primTy primVal ->
  Either (Error IR.NoExt ext primTy primVal) (IR.Value primTy primVal)

data ExtFuns ty ext' ext primTy primVal
  = ExtFuns
      { tExtFun :: TermExtFun ty ext' ext primTy primVal,
        eExtFun :: ElimExtFun ty ext' ext primTy primVal
      }

rejectExts :: ExtFuns ty ext ext' primTy primVal
rejectExts =
  ExtFuns
    { tExtFun = \_ -> Left . UnsupportedTermExt,
      eExtFun = \_ -> Left . UnsupportedElimExt
    }

type LookupFun ty ext primTy primVal =
  IR.GlobalName -> Maybe (IR.GlobalWith (ty IR.NoExt) ext primTy primVal)

type NoExtensions ext primTy primVal =
  ( IR.TermX ext primTy primVal ~ Void,
    IR.ElimX ext primTy primVal ~ Void
  )

type EvalPatSubst ext primTy primVal =
  ( HasPatSubst (OnlyExts.T ext) primTy primVal (IR.TermX ext primTy primVal),
    HasPatSubst (OnlyExts.T ext) primTy primVal (IR.ElimX ext primTy primVal),
    -- FIXME?
    HasPatSubstTerm (OnlyExts.T ext) primTy primVal primTy,
    HasPatSubstTerm (OnlyExts.T ext) primTy primVal primVal
  )

-- |
-- * @extT@: extension of current term
-- * @extG@: extension of terms in globals
type CanEval extT extG primTy primVal =
  ( Param.CanApply primTy,
    Param.CanApply primVal,
    EvalPatSubst extT primTy primVal,
    -- no extensions (only annotations) allowed in global context
    NoExtensions extG primTy primVal,
    HasSubstValue IR.NoExt primTy primVal primTy,
    HasSubstValue IR.NoExt primTy primVal primVal
  )

-- annotations are discarded
evalTermWith ::
  ( CanEval extT extT' primTy primVal,
    IR.ToTerm ty IR.NoExt IR.NoExt
  ) =>
  LookupFun ty extT' primTy primVal ->
  ExtFuns ty extT' extT primTy primVal ->
  IR.Term' (OnlyExts.T extT) primTy primVal ->
  Either (Error IR.NoExt extT primTy primVal) (IR.Value primTy primVal)
evalTermWith _ _ (IR.Star' u _) =
  pure $ IR.VStar u
evalTermWith _ _ (IR.PrimTy' p _) =
  pure $ IR.VPrimTy p
evalTermWith _ _ (IR.Prim' p _) =
  pure $ IR.VPrim p
evalTermWith g exts (IR.Pi' π s t _) =
  IR.VPi π <$> evalTermWith g exts s <*> evalTermWith g exts t
evalTermWith g exts (IR.Lam' t _) =
  IR.VLam <$> evalTermWith g exts t
evalTermWith g exts (IR.Sig' π s t _) =
  IR.VSig π <$> evalTermWith g exts s <*> evalTermWith g exts t
evalTermWith g exts (IR.Pair' s t _) =
  IR.VPair <$> evalTermWith g exts s <*> evalTermWith g exts t
evalTermWith _ _ (IR.UnitTy' _) =
  pure IR.VUnitTy
evalTermWith _ _ (IR.Unit' _) =
  pure IR.VUnit
evalTermWith g exts (IR.Let' _ l b _) = do
  l' <- evalElimWith g exts l
  b' <- evalTermWith g exts b
  substV l' b'
evalTermWith g exts (IR.Elim' e _) =
  evalElimWith g exts e
evalTermWith g exts (IR.TermX a) =
  tExtFun exts g a

evalElimWith ::
  ( CanEval extT extT' primTy primVal,
    IR.ToTerm ty IR.NoExt IR.NoExt
  ) =>
  LookupFun ty extT' primTy primVal ->
  ExtFuns ty extT' extT primTy primVal ->
  IR.Elim' (OnlyExts.T extT) primTy primVal ->
  Either (Error IR.NoExt extT primTy primVal) (IR.Value primTy primVal)
evalElimWith _ _ (IR.Bound' i _) =
  pure $ IR.VBound i
evalElimWith g exts (IR.Free' x _)
  | IR.Global x <- x,
    Just e <- toLambda =<< g x =
    evalElimWith g exts e
  | otherwise = pure $ IR.VFree x
evalElimWith g exts (IR.App' s t _) =
  join $
    vapp <$> evalElimWith g exts s
      <*> evalTermWith g exts t
      <*> pure ()
evalElimWith g exts (IR.Ann' _ s _ _ _) =
  evalTermWith g exts s
evalElimWith g exts (IR.ElimX a) =
  eExtFun exts g a

evalTerm ::
  ( CanEval extT extT' primTy primVal,
    IR.ToTerm ty IR.NoExt IR.NoExt
  ) =>
  LookupFun ty extT' primTy primVal ->
  IR.Term' extT primTy primVal ->
  Either (Error IR.NoExt extT primTy primVal) (IR.Value primTy primVal)
evalTerm g t = evalTermWith g rejectExts $ OnlyExts.onlyExtsT t

evalElim ::
  ( CanEval extT extT' primTy primVal,
    IR.ToTerm ty IR.NoExt IR.NoExt
  ) =>
  LookupFun ty extT' primTy primVal ->
  IR.Elim' extT primTy primVal ->
  Either (Error IR.NoExt extT primTy primVal) (IR.Value primTy primVal)
evalElim g e = evalElimWith g rejectExts $ OnlyExts.onlyExtsE e

-- TODO generalise the @IR.NoExt@s
toLambda ::
  forall ty ext ext' primTy primVal.
  ( EvalPatSubst ext' primTy primVal,
    NoExtensions ext primTy primVal,
    IR.ToTerm ty IR.NoExt IR.NoExt
  ) =>
  IR.GlobalWith (ty IR.NoExt) ext primTy primVal ->
  Maybe (IR.Elim' (OnlyExts.T ext') primTy primVal)
toLambda (IR.GFunction (IR.Function {funUsage, funType, funClauses}))
  | IR.FunClause pats rhs :| [] <- funClauses = do
    patVars <- traverse singleVar pats
    let len = fromIntegral $ length patVars
    let vars = map bound [len - 1, len - 2 .. 0]
    let patMap = IntMap.fromList $ zip patVars vars
    let transform = extTransformT $ OnlyExts.injector `compose` forgetter
    let π = IR.globalToUsage funUsage
    let ty = OnlyExts.injectT $ IR.toTerm funType
    case patSubst patMap $ weakBy len $ transform rhs of
      Left _ -> Nothing
      Right x -> pure $ IR.Ann π (applyN len lam x) ty 0 -- FIXME universe
  where
    singleVar (IR.PVar' p _) = Just p
    singleVar _ = Nothing
    applyN 0 _ x = x
    applyN n f x = applyN (n - 1) f (f $! x)
    bound :: IR.BoundVar -> IR.Elim' (OnlyExts.T ext') primTy primVal
    bound x = IR.Bound' x ()
    lam ::
      IR.Term' (OnlyExts.T z) primTy primVal ->
      IR.Term' (OnlyExts.T z) primTy primVal
    lam x = IR.Lam' x ()
toLambda _ = Nothing

class GHasWeak f where
  gweakBy' :: Natural -> IR.BoundVar -> f t -> f t

instance GHasWeak U1 where gweakBy' _ _ U1 = U1

instance GHasWeak V1 where gweakBy' _ _ v = case v of

instance (GHasWeak f, GHasWeak g) => GHasWeak (f :*: g) where
  gweakBy' b i (x :*: y) = gweakBy' b i x :*: gweakBy' b i y

instance (GHasWeak f, GHasWeak g) => GHasWeak (f :+: g) where
  gweakBy' b i (L1 x) = L1 (gweakBy' b i x)
  gweakBy' b i (R1 x) = R1 (gweakBy' b i x)

instance GHasWeak f => GHasWeak (M1 i t f) where
  gweakBy' b i (M1 x) = M1 (gweakBy' b i x)

instance HasWeak f => GHasWeak (K1 k f) where
  gweakBy' b i (K1 x) = K1 (weakBy' b i x)

instance HasWeak ()

instance HasWeak Void

instance HasWeak Natural where weakBy' _ _ n = n

instance HasWeak Usage.T where weakBy' _ _ π = π

instance (HasWeak a, HasWeak b) => HasWeak (a, b)

instance (HasWeak a, HasWeak b, HasWeak c) => HasWeak (a, b, c)

instance (HasWeak a, HasWeak b) => HasWeak (Either a b)

instance HasWeak a => HasWeak (Maybe a)

instance HasWeak a => HasWeak [a]

instance HasWeak a => HasWeak (NonEmpty a)

instance HasWeak Symbol where weakBy' _ _ x = x

class GHasWeak f => GHasSubst ext primTy primVal f where
  gsubstWith ::
    -- | How many bindings have been traversed so far
    Natural ->
    -- | Variable to substitute
    IR.BoundVar ->
    -- | Expression to substitute with
    IR.Elim' ext primTy primVal ->
    f t ->
    f t

instance GHasSubst ext primTy primVal U1 where gsubstWith _ _ _ U1 = U1

instance GHasSubst ext primTy primVal V1 where
  gsubstWith _ _ _ v = case v of

instance
  ( GHasSubst ext primTy primVal f,
    GHasSubst ext primTy primVal g
  ) =>
  GHasSubst ext primTy primVal (f :*: g)
  where
  gsubstWith b i e (x :*: y) = gsubstWith b i e x :*: gsubstWith b i e y

instance
  ( GHasSubst ext primTy primVal f,
    GHasSubst ext primTy primVal g
  ) =>
  GHasSubst ext primTy primVal (f :+: g)
  where
  gsubstWith b i e (L1 x) = L1 (gsubstWith b i e x)
  gsubstWith b i e (R1 x) = R1 (gsubstWith b i e x)

instance
  GHasSubst ext primTy primVal f =>
  GHasSubst ext primTy primVal (M1 i t f)
  where
  gsubstWith b i e (M1 x) = M1 (gsubstWith b i e x)

instance
  HasSubst ext primTy primVal f =>
  GHasSubst ext primTy primVal (K1 k f)
  where
  gsubstWith b i e (K1 x) = K1 (substWith b i e x)

instance HasSubst ext primTy primVal ()

instance HasSubst ext primTy primVal Void

instance HasSubst ext primTy primVal Natural where substWith _ _ _ n = n

instance HasSubst ext primTy primVal Usage.T where substWith _ _ _ π = π

instance
  ( HasSubst ext primTy primVal a,
    HasSubst ext primTy primVal b
  ) =>
  HasSubst ext primTy primVal (a, b)

instance
  ( HasSubst ext primTy primVal a,
    HasSubst ext primTy primVal b,
    HasSubst ext primTy primVal c
  ) =>
  HasSubst ext primTy primVal (a, b, c)

instance
  ( HasSubst ext primTy primVal a,
    HasSubst ext primTy primVal b
  ) =>
  HasSubst ext primTy primVal (Either a b)

instance
  HasSubst ext primTy primVal a =>
  HasSubst ext primTy primVal (Maybe a)

instance
  HasSubst ext primTy primVal a =>
  HasSubst ext primTy primVal [a]

instance
  HasSubst ext primTy primVal a =>
  HasSubst ext primTy primVal (NonEmpty a)

instance HasSubst ext primTy primVal Symbol where
  substWith _ _ _ x = x

class GHasWeak f => GHasSubstV extV primTy primVal f where
  gsubstVWith ::
    Natural ->
    IR.BoundVar ->
    IR.Value' extV primTy primVal ->
    f t ->
    Either (Error extV extT primTy primVal) (f t)

instance GHasSubstV ext primTy primVal U1 where gsubstVWith _ _ _ U1 = pure U1

instance GHasSubstV ext primTy primVal V1 where
  gsubstVWith _ _ _ v = case v of

instance
  ( GHasSubstV ext primTy primVal f,
    GHasSubstV ext primTy primVal g
  ) =>
  GHasSubstV ext primTy primVal (f :*: g)
  where
  gsubstVWith b i e (x :*: y) =
    (:*:) <$> gsubstVWith b i e x
      <*> gsubstVWith b i e y

instance
  ( GHasSubstV ext primTy primVal f,
    GHasSubstV ext primTy primVal g
  ) =>
  GHasSubstV ext primTy primVal (f :+: g)
  where
  gsubstVWith b i e (L1 x) = L1 <$> gsubstVWith b i e x
  gsubstVWith b i e (R1 x) = R1 <$> gsubstVWith b i e x

instance
  GHasSubstV ext primTy primVal f =>
  GHasSubstV ext primTy primVal (M1 i t f)
  where
  gsubstVWith b i e (M1 x) = M1 <$> gsubstVWith b i e x

instance
  HasSubstV ext primTy primVal f =>
  GHasSubstV ext primTy primVal (K1 k f)
  where
  gsubstVWith b i e (K1 x) = K1 <$> substVWith b i e x

instance HasSubstV ext primTy primVal ()

instance HasSubstV ext primTy primVal Void

instance HasSubstV ext primTy primVal Natural where
  substVWith _ _ _ n = pure n

instance HasSubstV ext primTy primVal Usage.T where
  substVWith _ _ _ π = pure π

instance
  ( HasSubstV ext primTy primVal a,
    HasSubstV ext primTy primVal b
  ) =>
  HasSubstV ext primTy primVal (a, b)

instance
  ( HasSubstV ext primTy primVal a,
    HasSubstV ext primTy primVal b,
    HasSubstV ext primTy primVal c
  ) =>
  HasSubstV ext primTy primVal (a, b, c)

instance
  ( HasSubstV ext primTy primVal a,
    HasSubstV ext primTy primVal b
  ) =>
  HasSubstV ext primTy primVal (Either a b)

instance
  HasSubstV ext primTy primVal a =>
  HasSubstV ext primTy primVal (Maybe a)

instance
  HasSubstV ext primTy primVal a =>
  HasSubstV ext primTy primVal [a]

instance
  HasSubstV ext primTy primVal a =>
  HasSubstV ext primTy primVal (NonEmpty a)

instance HasSubstV ext primTy primVal Symbol where
  substVWith _ _ _ x = pure x

class GHasWeak f => GHasPatSubst extT primTy primVal f where
  gpatSubst' ::
    -- | How many bindings have been traversed so far
    Natural ->
    -- | Mapping of pattern variables to matched subterms
    IR.PatternMap (IR.Elim' extT primTy primVal) ->
    f t ->
    Either IR.PatternVar (f t)

instance GHasPatSubst ext primTy primVal U1 where gpatSubst' _ _ U1 = pure U1

instance GHasPatSubst ext primTy primVal V1 where
  gpatSubst' _ _ v = case v of

instance
  ( GHasPatSubst ext primTy primVal f,
    GHasPatSubst ext primTy primVal g
  ) =>
  GHasPatSubst ext primTy primVal (f :*: g)
  where
  gpatSubst' b m (x :*: y) =
    (:*:) <$> gpatSubst' b m x
      <*> gpatSubst' b m y

instance
  ( GHasPatSubst ext primTy primVal f,
    GHasPatSubst ext primTy primVal g
  ) =>
  GHasPatSubst ext primTy primVal (f :+: g)
  where
  gpatSubst' b m (L1 x) = L1 <$> gpatSubst' b m x
  gpatSubst' b m (R1 x) = R1 <$> gpatSubst' b m x

instance
  GHasPatSubst ext primTy primVal f =>
  GHasPatSubst ext primTy primVal (M1 i t f)
  where
  gpatSubst' b m (M1 x) = M1 <$> gpatSubst' b m x

instance
  HasPatSubst ext primTy primVal f =>
  GHasPatSubst ext primTy primVal (K1 k f)
  where
  gpatSubst' b m (K1 x) = K1 <$> patSubst' b m x

instance HasPatSubst ext primTy primVal ()

instance HasPatSubst ext primTy primVal Void

instance HasPatSubst ext primTy primVal Natural where
  patSubst' _ _ n = pure n

instance HasPatSubst ext primTy primVal Usage.T where
  patSubst' _ _ π = pure π

instance
  ( HasPatSubst ext primTy primVal a,
    HasPatSubst ext primTy primVal b
  ) =>
  HasPatSubst ext primTy primVal (a, b)

instance
  ( HasPatSubst ext primTy primVal a,
    HasPatSubst ext primTy primVal b,
    HasPatSubst ext primTy primVal c
  ) =>
  HasPatSubst ext primTy primVal (a, b, c)

instance
  ( HasPatSubst ext primTy primVal a,
    HasPatSubst ext primTy primVal b
  ) =>
  HasPatSubst ext primTy primVal (Either a b)

instance
  HasPatSubst ext primTy primVal a =>
  HasPatSubst ext primTy primVal (Maybe a)

instance
  HasPatSubst ext primTy primVal a =>
  HasPatSubst ext primTy primVal [a]

instance
  HasPatSubst ext primTy primVal a =>
  HasPatSubst ext primTy primVal (NonEmpty a)

instance (HasWeak ty, HasWeak term) => HasWeak (App.Take ty term)

instance
  (HasWeak ty, HasWeak term, HasWeak (App.ParamVar ext)) =>
  HasWeak (App.Arg' ext ty term)

instance
  (HasWeak ty, HasWeak term, HasWeak (App.ParamVar ext)) =>
  HasWeak (App.Return' ext ty term)

instance HasWeak App.DeBruijn where
  weakBy' b i (App.BoundVar j) =
    App.BoundVar $ if j >= i then j + b else j
  weakBy' _ _ (App.FreeVar x) = App.FreeVar x

instance
  (HasSubst ext primTy primVal ty, HasSubst ext primTy primVal term) =>
  HasSubst ext primTy primVal (App.Take ty term)

instance
  ( HasSubst ext primTy primVal term,
    HasSubst ext primTy primVal ty,
    HasSubst ext primTy primVal (App.ParamVar ext)
  ) =>
  HasSubst ext primTy primVal (App.Arg' ext ty term)

instance
  ( HasSubst ext primTy primVal ty,
    HasSubst ext primTy primVal term,
    HasSubst ext primTy primVal (App.ParamVar ext)
  ) =>
  HasSubst ext primTy primVal (App.Return' ext ty term)

instance
  ( HasPatSubst ext primTy primVal ty,
    HasPatSubst ext primTy primVal term
  ) =>
  HasPatSubst ext primTy primVal (App.Take ty term)

instance
  ( HasPatSubst ext primTy primVal term,
    HasPatSubst ext primTy primVal ty,
    HasPatSubst ext primTy primVal (App.ParamVar ext)
  ) =>
  HasPatSubst ext primTy primVal (App.Arg' ext ty term)

instance
  ( HasPatSubst ext primTy primVal ty,
    HasPatSubst ext primTy primVal term,
    HasPatSubst ext primTy primVal (App.ParamVar ext)
  ) =>
  HasPatSubst ext primTy primVal (App.Return' ext ty term)

instance
  AllSubst ext primTy primVal =>
  HasSubstTerm ext primTy primVal (IR.Term' ext primTy primVal)
  where
  substTermWith = substWith

instance
  ( AllSubst ext primTy primVal,
    Monoid (IR.XBound ext primTy primVal),
    Monoid (IR.XFree ext primTy primVal),
    Monoid (IR.XElim ext primTy primVal)
  ) =>
  HasSubstTerm ext primTy primVal App.DeBruijn
  where
  substTermWith b i e (App.BoundVar j) =
    IR.Elim' (substWith b i e (IR.Bound' j mempty)) mempty
  substTermWith _ _ _ (App.FreeVar x) =
    IR.Elim' (IR.Free' (IR.Global x) mempty) mempty

instance
  ( HasWeak ty,
    HasSubstTerm ext primTy primVal term
  ) =>
  HasSubstTerm ext primTy primVal (App.Take ty term)
  where
  substTermWith b i e (App.Take {term}) = substTermWith b i e term

instance
  ( HasSubstTerm ext primTy primVal (App.ParamVar ext),
    HasWeak ty,
    HasSubstTerm ext primTy primVal term
  ) =>
  HasSubstTerm ext primTy primVal (App.Arg' ext ty term)
  where
  substTermWith b i e (App.VarArg x) = substTermWith b i e x
  substTermWith b i e (App.TermArg t) = substTermWith b i e t

substTake ::
  HasSubstTerm ext primTy primVal term =>
  IR.BoundVar ->
  IR.BoundVar ->
  IR.Elim' ext primTy primVal ->
  App.Take ty term ->
  IR.Term' ext primTy primVal
substTake b i e (App.Take {term}) = substTermWith b i e term

instance
  ( AllSubstV extV primTy primVal,
    Monoid (IR.XVNeutral extV primTy primVal),
    Monoid (IR.XVLam extV primTy primVal),
    Monoid (IR.XVPrimTy extV primTy primVal),
    Monoid (IR.XVPrim extV primTy primVal)
  ) =>
  HasSubstValue extV primTy primVal (IR.Value' extV primTy primVal)
  where
  substValueWith = substVWith

instance
  ( AllSubstV ext primTy primVal,
    Monoid (IR.XNBound ext primTy primVal),
    Monoid (IR.XNFree ext primTy primVal),
    Monoid (IR.XVNeutral ext primTy primVal),
    Monoid (IR.XVLam ext primTy primVal),
    Monoid (IR.XVPrimTy ext primTy primVal),
    Monoid (IR.XVPrim ext primTy primVal)
  ) =>
  HasSubstValue ext primTy primVal App.DeBruijn
  where
  substValueWith b i e (App.BoundVar j) =
    substNeutralWith b i e (IR.NBound' j mempty) mempty
  substValueWith _ _ _ (App.FreeVar x) =
    pure $ IR.VNeutral' (IR.NFree' (IR.Global x) mempty) mempty

instance
  ( HasWeak ty,
    HasSubstValue ext primTy primVal term
  ) =>
  HasSubstValue ext primTy primVal (App.Take ty term)
  where
  substValueWith b i e (App.Take {term}) = substValueWith b i e term

instance
  ( HasSubstValue ext primTy primVal (App.ParamVar ext),
    HasSubstValue ext primTy primVal ty,
    HasSubstValue ext primTy primVal term
  ) =>
  HasSubstValue ext primTy primVal (App.Arg' ext ty term)
  where
  substValueWith b i e (App.VarArg x) = substValueWith b i e x
  substValueWith b i e (App.TermArg t) = substValueWith b i e t

instance
  ( HasSubstValue ext primTy primVal a,
    IR.ValueAll HasWeak ext primTy primVal,
    IR.NeutralAll HasWeak ext primTy primVal,
    HasWeak primTy,
    HasWeak primVal,
    Monoid (IR.XVPi ext primTy primVal)
  ) =>
  HasSubstValue ext primTy primVal (NonEmpty a)
  where
  substValueWith b i e tys =
    foldr1 pi <$> traverse (substValueWith b i e) tys
    where
      pi s t = IR.VPi' Usage.Omega s (weak t) mempty

-- TODO generalise @IR.NoExt@
instance
  ( HasWeak primTy,
    HasWeak primVal,
    HasSubstValue IR.NoExt primTy (Param.TypedPrim primTy primVal) primTy,
    Param.CanApply primTy,
    Param.CanApply (Param.TypedPrim primTy primVal)
  ) =>
  HasSubstValue IR.NoExt primTy
    (Param.TypedPrim primTy primVal)
    (Param.TypedPrim primTy primVal)
  where
  substValueWith b i e (App.Cont {fun, args}) = do
    let app f x = vapp f x ()
    let fun' = IR.VPrim (App.takeToReturn fun)
    args' <- traverse (substValueWith b i e . argToValue) args
    foldlM app fun' args'
  substValueWith _ _ _ ret@(App.Return {}) =
    pure $ IR.VPrim ret

argToValue ::
  App.Arg (Param.PrimType primTy) primVal ->
  IR.Value primTy (Param.TypedPrim primTy primVal)
argToValue = \case
  App.TermArg (App.Take {type', term}) ->
    IR.VPrim $ App.Return {retType = type', retTerm = term}
  App.BoundArg i -> IR.VBound i
  App.FreeArg x -> IR.VFree $ IR.Global x

instance
  ( HasWeak primTy,
    HasWeak primVal
  ) =>
  HasPatSubstTerm (OnlyExts.T ext) primTy
    (Param.TypedPrim primTy primVal)
    (Param.TypedPrim primTy primVal)
  where
  -- FIXME pat vars can't yet show up here
  patSubstTerm' _ _ (App.Cont {fun, args}) =
    pure $ IR.Elim $ foldl IR.App (takeToElim fun) (map argToTerm args)
  patSubstTerm' _ _ ret@(App.Return {}) =
    pure $ IR.Prim ret

takeToElim ::
  App.Take (Param.PrimType primTy) primVal ->
  IR.Elim' (OnlyExts.T ext) primTy (Param.TypedPrim primTy primVal)
takeToElim (App.Take {type', term}) =
  let term' = IR.Prim (App.Return {retType = type', retTerm = term})
      ty' = typeToTerm type'
   in IR.Ann Usage.Omega term' ty' 0

argToTerm ::
  App.Arg (Param.PrimType primTy) primVal ->
  IR.Term' (OnlyExts.T ext) primTy (Param.TypedPrim primTy primVal)
argToTerm = \case
  App.TermArg (App.Take {type', term}) ->
    IR.Prim $ App.Return {retType = type', retTerm = term}
  App.BoundArg i -> IR.Elim $ IR.Bound i
  App.FreeArg x -> IR.Elim $ IR.Free $ IR.Global x

typeToTerm ::
  ( Monoid (IR.XPi ext primTy primVal),
    Monoid (IR.XPrimTy ext primTy primVal)
  ) =>
  Param.PrimType primTy ->
  IR.Term' ext primTy primVal
typeToTerm tys = foldr1 arr $ map prim tys
  where
    prim ty = IR.PrimTy' ty mempty
    arr s t = IR.Pi' Usage.Omega s t mempty
