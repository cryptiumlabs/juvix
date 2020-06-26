{-# LANGUAGE UndecidableInstances #-}

-- |
-- This includes the evaluators (evalTerm and evalElim),
-- the value application function (vapp) and
-- the substitution functions (substTerm and substElim).
module Juvix.Core.IR.Evaluator where

import qualified Juvix.Core.IR.Typechecker.Types as TC
import qualified Juvix.Core.IR.Types as IR
import qualified Juvix.Core.IR.Types.Base as IR
import qualified Juvix.Core.Parameterisation as Param
import Juvix.Library

class HasWeak a where
  weakBy' :: Natural -> IR.BoundVar -> a -> a

weakBy :: HasWeak a => Natural -> a -> a
weakBy b = weakBy' b 0

weak' :: HasWeak a => IR.BoundVar -> a -> a
weak' = weakBy' 1

weak :: HasWeak a => a -> a
weak = weak' 0

instance HasWeak () where weakBy' _ _ () = ()

instance HasWeak Void where weakBy' _ _ v = absurd v

type AllWeak ext primTy primVal =
  ( IR.TermAll HasWeak ext primTy primVal,
    IR.ElimAll HasWeak ext primTy primVal
  )

instance AllWeak ext primTy primVal => HasWeak (IR.Term' ext primTy primVal) where
  weakBy' b i (IR.Star' u a) =
    IR.Star' u (weakBy' b i a)
  weakBy' b i (IR.PrimTy' p a) =
    IR.PrimTy' p (weakBy' b i a)
  weakBy' b i (IR.Pi' π s t a) =
    IR.Pi' π (weakBy' b i s) (weakBy' b (succ i) t) (weakBy' b i a)
  weakBy' b i (IR.Lam' t a) =
    IR.Lam' (weakBy' b (succ i) t) (weakBy' b i a)
  weakBy' b i (IR.Let' π s t a) =
    IR.Let' π (weakBy' b i s) (weakBy' b (succ i) t) (weakBy' b i a)
  weakBy' b i (IR.Elim' f a) =
    IR.Elim' (weakBy' b i f) (weakBy' b i a)
  weakBy' b i (IR.TermX a) =
    IR.TermX (weakBy' b i a)

weakTermBy' ::
  AllWeak ext primTy primVal =>
  Natural ->
  IR.BoundVar ->
  IR.Term' ext primTy primVal ->
  IR.Term' ext primTy primVal
weakTermBy' = weakBy'

weakTermBy ::
  AllWeak ext primTy primVal =>
  Natural ->
  IR.Term' ext primTy primVal ->
  IR.Term' ext primTy primVal
weakTermBy = weakBy

weakTerm' ::
  AllWeak ext primTy primVal =>
  IR.BoundVar ->
  IR.Term' ext primTy primVal ->
  IR.Term' ext primTy primVal
weakTerm' = weak'

weakTerm ::
  AllWeak ext primTy primVal =>
  IR.Term' ext primTy primVal ->
  IR.Term' ext primTy primVal
weakTerm = weak

instance AllWeak ext primTy primVal => HasWeak (IR.Elim' ext primTy primVal) where
  weakBy' b i (IR.Bound' j a)
    | j >= i = IR.Bound' (j + b) a'
    | otherwise = IR.Bound' j a'
    where
      a' = weakBy' b i a
  weakBy' b i (IR.Free' x a) =
    IR.Free' x (weakBy' b i a)
  weakBy' b i (IR.Prim' p a) =
    IR.Prim' p (weakBy' b i a)
  weakBy' b i (IR.App' s t a) =
    IR.App' (weakBy' b i s) (weakBy' b i t) (weakBy' b i a)
  weakBy' b i (IR.Ann' π s t l a) =
    IR.Ann' π (weakBy' b i s) (weakBy' b i t) l (weakBy' b i a)
  weakBy' b i (IR.ElimX a) =
    IR.ElimX (weakBy' b i a)

weakElimBy' ::
  AllWeak ext primTy primVal =>
  Natural ->
  IR.BoundVar ->
  IR.Elim' ext primTy primVal ->
  IR.Elim' ext primTy primVal
weakElimBy' = weakBy'

weakElimBy ::
  AllWeak ext primTy primVal =>
  Natural ->
  IR.Elim' ext primTy primVal ->
  IR.Elim' ext primTy primVal
weakElimBy = weakBy

weakElim' ::
  AllWeak ext primTy primVal =>
  IR.BoundVar ->
  IR.Elim' ext primTy primVal ->
  IR.Elim' ext primTy primVal
weakElim' = weak'

weakElim ::
  AllWeak ext primTy primVal =>
  IR.Elim' ext primTy primVal ->
  IR.Elim' ext primTy primVal
weakElim = weak

class HasWeak a => HasSubst ext primTy primVal a where
  subst' :: IR.BoundVar -> IR.Elim' ext primTy primVal -> a -> a

subst ::
  HasSubst ext primTy primVal a =>
  IR.Elim' ext primTy primVal ->
  a ->
  a
subst = subst' 0

instance HasSubst ext primTy primVal () where subst' _ _ () = ()

instance HasSubst ext primTy primVal Void where subst' _ _ v = absurd v

type AllSubst ext primTy primVal =
  ( IR.TermAll (HasSubst ext primTy primVal) ext primTy primVal,
    IR.ElimAll (HasSubst ext primTy primVal) ext primTy primVal
  )

instance
  AllSubst ext primTy primVal =>
  HasSubst ext primTy primVal (IR.Term' ext primTy primVal)
  where
  subst' i e (IR.Star' u a) =
    IR.Star' u (subst' i e a)
  subst' i e (IR.PrimTy' t a) =
    IR.PrimTy' t (subst' i e a)
  subst' i e (IR.Pi' π s t a) =
    IR.Pi' π (subst' i e s) (subst' (succ i) (weak' i e) t) (subst' i e a)
  subst' i e (IR.Lam' t a) =
    IR.Lam' (subst' (succ i) (weak' i e) t) (subst' i e a)
  subst' i e (IR.Let' π l b a) =
    IR.Let' π (subst' i e l) (subst' (succ i) (weak' i e) b) (subst' i e a)
  subst' i e (IR.Elim' t a) =
    IR.Elim' (subst' i e t) (subst' i e a)
  subst' i e (IR.TermX a) =
    IR.TermX (subst' i e a)

substTerm' ::
  AllSubst ext primTy primVal =>
  IR.BoundVar ->
  IR.Elim' ext primTy primVal ->
  IR.Term' ext primTy primVal ->
  IR.Term' ext primTy primVal
substTerm' = subst'

substTerm ::
  AllSubst ext primTy primVal =>
  IR.Elim' ext primTy primVal ->
  IR.Term' ext primTy primVal ->
  IR.Term' ext primTy primVal
substTerm = subst

instance
  AllSubst ext primTy primVal =>
  HasSubst ext primTy primVal (IR.Elim' ext primTy primVal)
  where
  subst' i e (IR.Bound' j a) =
    case compare j i of
      LT -> IR.Bound' j a'
      EQ -> e
      GT -> IR.Bound' (pred j) a'
    where
      a' = subst' i e a
  subst' i e (IR.Free' x a) =
    IR.Free' x (subst' i e a)
  subst' i e (IR.Prim' p a) =
    IR.Prim' p (subst' i e a)
  subst' i e (IR.App' f s a) =
    IR.App' (subst' i e f) (subst' i e s) (subst' i e a)
  subst' i e (IR.Ann' π s t l a) =
    IR.Ann' π (subst' i e s) (subst' i e t) l (subst' i e a)
  subst' i e (IR.ElimX a) =
    IR.ElimX (subst' i e a)

substElim' ::
  AllSubst ext primTy primVal =>
  IR.BoundVar ->
  IR.Elim' ext primTy primVal ->
  IR.Elim' ext primTy primVal ->
  IR.Elim' ext primTy primVal
substElim' = subst'

substElim ::
  AllSubst ext primTy primVal =>
  IR.Elim' ext primTy primVal ->
  IR.Elim' ext primTy primVal ->
  IR.Elim' ext primTy primVal
substElim = subst

type AllWeakV ext primTy primVal =
  ( IR.ValueAll HasWeak ext primTy primVal,
    IR.NeutralAll HasWeak ext primTy primVal
  )

instance
  AllWeakV ext primTy primVal =>
  HasWeak (IR.Value' ext primTy primVal)
  where
  weakBy' b i (IR.VStar' n a) =
    IR.VStar' n (weakBy' b i a)
  weakBy' b i (IR.VPrimTy' p a) =
    IR.VPrimTy' p (weakBy' b i a)
  weakBy' b i (IR.VPi' π s t a) =
    IR.VPi' π (weakBy' b i s) (weakBy' b (succ i) t) (weakBy' b i a)
  weakBy' b i (IR.VLam' t a) =
    IR.VLam' (weakBy' b (succ i) t) (weakBy' b i a)
  weakBy' b i (IR.VNeutral' n a) =
    IR.VNeutral' (weakBy' b i n) (weakBy' b i a)
  weakBy' b i (IR.VPrim' p a) =
    IR.VPrim' p (weakBy' b i a)
  weakBy' b i (IR.ValueX a) =
    IR.ValueX (weakBy' b i a)

weakValueBy' ::
  AllWeakV ext primTy primVal =>
  Natural ->
  IR.BoundVar ->
  IR.Value' ext primTy primVal ->
  IR.Value' ext primTy primVal
weakValueBy' = weakBy'

weakValueBy ::
  AllWeakV ext primTy primVal =>
  Natural ->
  IR.Value' ext primTy primVal ->
  IR.Value' ext primTy primVal
weakValueBy = weakBy

weakValue' ::
  AllWeakV ext primTy primVal =>
  IR.BoundVar ->
  IR.Value' ext primTy primVal ->
  IR.Value' ext primTy primVal
weakValue' = weak'

weakValue ::
  AllWeakV ext primTy primVal =>
  IR.Value' ext primTy primVal ->
  IR.Value' ext primTy primVal
weakValue = weak

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

weakNeutralBy' ::
  AllWeakV ext primTy primVal =>
  Natural ->
  IR.BoundVar ->
  IR.Neutral' ext primTy primVal ->
  IR.Neutral' ext primTy primVal
weakNeutralBy' = weakBy'

weakNeutralBy ::
  AllWeakV ext primTy primVal =>
  Natural ->
  IR.Neutral' ext primTy primVal ->
  IR.Neutral' ext primTy primVal
weakNeutralBy = weakBy

weakNeutral' ::
  AllWeakV ext primTy primVal =>
  IR.BoundVar ->
  IR.Neutral' ext primTy primVal ->
  IR.Neutral' ext primTy primVal
weakNeutral' = weak'

weakNeutral ::
  AllWeakV ext primTy primVal =>
  IR.Neutral' ext primTy primVal ->
  IR.Neutral' ext primTy primVal
weakNeutral = weak

class HasWeak a => HasSubstV ext primTy primVal a where
  substV' ::
    TC.HasThrowTC' ext primTy primVal m =>
    Param.Parameterisation primTy primVal ->
    IR.BoundVar ->
    IR.Value' ext primTy primVal ->
    a ->
    m a

substV ::
  ( HasSubstV ext primTy primVal a,
    TC.HasThrowTC' ext primTy primVal m
  ) =>
  Param.Parameterisation primTy primVal ->
  IR.Value' ext primTy primVal ->
  a ->
  m a
substV param = substV' param 0

type AllSubstV ext primTy primVal =
  ( IR.ValueAll (HasSubstV ext primTy primVal) ext primTy primVal,
    IR.NeutralAll (HasSubstV ext primTy primVal) ext primTy primVal
  )

instance HasSubstV ext primTy primVal () where substV' _ _ _ = pure

instance HasSubstV ext primTy primVal Void where substV' _ _ _ = absurd

instance
  ( AllSubstV ext primTy primVal,
    Monoid (IR.XVNeutral ext primTy primVal),
    Monoid (IR.XVLam ext primTy primVal),
    Monoid (IR.XVPrim ext primTy primVal)
  ) =>
  HasSubstV ext primTy primVal (IR.Value' ext primTy primVal)
  where
  substV' param i e (IR.VStar' n a) =
    IR.VStar' n <$> substV' param i e a
  substV' param i e (IR.VPrimTy' p a) =
    IR.VPrimTy' p <$> substV' param i e a
  substV' param i e (IR.VPi' π s t a) =
    IR.VPi' π <$> substV' param i e s
      <*> substV' param (succ i) (weak' i e) t
      <*> substV' param i e a
  substV' param i e (IR.VLam' t a) =
    IR.VLam' <$> substV' param (succ i) (weak' i e) t
      <*> substV' param i e a
  substV' param i e (IR.VNeutral' n a) =
    substNeutral' param i e n a
  substV' param i e (IR.VPrim' p a) =
    IR.VPrim' p <$> substV' param i e a
  substV' param i e (IR.ValueX a) =
    IR.ValueX <$> substV' param i e a

substValue' ::
  ( AllSubstV ext primTy primVal,
    TC.HasThrowTC' ext primTy primVal m,
    Monoid (IR.XVNeutral ext primTy primVal),
    Monoid (IR.XVLam ext primTy primVal),
    Monoid (IR.XVPrim ext primTy primVal)
  ) =>
  Param.Parameterisation primTy primVal ->
  IR.BoundVar ->
  IR.Value' ext primTy primVal ->
  IR.Value' ext primTy primVal ->
  m (IR.Value' ext primTy primVal)
substValue' = substV'

substValue ::
  ( AllSubstV ext primTy primVal,
    TC.HasThrowTC' ext primTy primVal m,
    Monoid (IR.XVNeutral ext primTy primVal),
    Monoid (IR.XVLam ext primTy primVal),
    Monoid (IR.XVPrim ext primTy primVal)
  ) =>
  Param.Parameterisation primTy primVal ->
  IR.Value' ext primTy primVal ->
  IR.Value' ext primTy primVal ->
  m (IR.Value' ext primTy primVal)
substValue param = substValue' param 0

substNeutral' ::
  ( AllSubstV ext primTy primVal,
    TC.HasThrowTC' ext primTy primVal m,
    Monoid (IR.XVNeutral ext primTy primVal),
    Monoid (IR.XVLam ext primTy primVal),
    Monoid (IR.XVPrim ext primTy primVal)
  ) =>
  Param.Parameterisation primTy primVal ->
  IR.BoundVar ->
  IR.Value' ext primTy primVal ->
  IR.Neutral' ext primTy primVal ->
  IR.XVNeutral ext primTy primVal ->
  m (IR.Value' ext primTy primVal) -- not Neutral'!!!
substNeutral' param i e (IR.NBound' j a) b = do
  a' <- substV' param i e a
  b' <- substV' param i e b
  pure $ case compare j i of
    LT -> IR.VNeutral' (IR.NBound' j a') b'
    EQ -> e
    GT -> IR.VNeutral' (IR.NBound' (pred j) a') b'
substNeutral' param i e (IR.NFree' x a) b =
  IR.VNeutral' <$> (IR.NFree' x <$> substV' param i e a)
    <*> substV' param i e b
substNeutral' param i e (IR.NApp' f s a) _ =
  join $
    vapp param <$> substNeutral' param i e f mempty
      <*> substV' param i e s
      <*> substV' param i e a
substNeutral' param i e (IR.NeutralX a) b =
  IR.VNeutral' <$> (IR.NeutralX <$> substV' param i e a)
    <*> substV' param i e b

substNeutral ::
  ( AllSubstV ext primTy primVal,
    TC.HasThrowTC' ext primTy primVal m,
    Monoid (IR.XVNeutral ext primTy primVal),
    Monoid (IR.XVLam ext primTy primVal),
    Monoid (IR.XVPrim ext primTy primVal)
  ) =>
  Param.Parameterisation primTy primVal ->
  IR.Value' ext primTy primVal ->
  IR.Neutral' ext primTy primVal ->
  IR.XVNeutral ext primTy primVal ->
  m (IR.Value' ext primTy primVal)
substNeutral param = substNeutral' param 0

vapp ::
  ( AllSubstV ext primTy primVal,
    TC.HasThrowTC' ext primTy primVal m,
    Monoid (IR.XVNeutral ext primTy primVal),
    Monoid (IR.XVLam ext primTy primVal),
    Monoid (IR.XVPrim ext primTy primVal)
  ) =>
  Param.Parameterisation primTy primVal ->
  IR.Value' ext primTy primVal ->
  IR.Value' ext primTy primVal ->
  -- | the annotation to use if the result is another application node
  -- (if it isn't, then this annotation is unused)
  IR.XNApp ext primTy primVal ->
  m (IR.Value' ext primTy primVal)
vapp param (IR.VLam' t _) s _ =
  substValue param s t
vapp _ (IR.VNeutral' f _) s b =
  pure $ IR.VNeutral' (IR.NApp' f s b) mempty
vapp param (IR.VPrim' p _) (IR.VPrim' q _) _
  | Just v <- Param.apply param p q =
    pure $ IR.VPrim' v mempty
vapp _ f x _ =
  TC.throwTC $ TC.CannotApply f x

type TermExtFun ext primTy primVal =
  IR.TermX ext primTy primVal -> IR.Value primTy primVal

type ElimExtFun ext primTy primVal =
  IR.ElimX ext primTy primVal -> IR.Value primTy primVal

type ExtFuns ext primTy primVal =
  (TermExtFun ext primTy primVal, ElimExtFun ext primTy primVal)

-- annotations are discarded
evalTermWith ::
  TC.HasThrowTC primTy primVal m =>
  ExtFuns ext primTy primVal ->
  Param.Parameterisation primTy primVal ->
  IR.Term' ext primTy primVal ->
  m (IR.Value primTy primVal)
evalTermWith _ _ (IR.Star' u _) =
  pure $ IR.VStar u
evalTermWith _ _ (IR.PrimTy' p _) =
  pure $ IR.VPrimTy p
evalTermWith exts param (IR.Pi' π s t _) =
  IR.VPi π <$> evalTermWith exts param s <*> evalTermWith exts param t
evalTermWith exts param (IR.Lam' t _) =
  IR.VLam <$> evalTermWith exts param t
evalTermWith exts param (IR.Let' _ l b _) = do
  l' <- evalElimWith exts param l
  b' <- evalTermWith exts param b
  substValue param l' b'
evalTermWith exts param (IR.Elim' e _) =
  evalElimWith exts param e
evalTermWith (tExt, _) _ (IR.TermX a) =
  pure $ tExt a

evalElimWith ::
  TC.HasThrowTC primTy primVal m =>
  ExtFuns ext primTy primVal ->
  Param.Parameterisation primTy primVal ->
  IR.Elim' ext primTy primVal ->
  m (IR.Value primTy primVal)
evalElimWith _ _ (IR.Bound' i _) =
  pure $ IR.VBound i
evalElimWith _ _ (IR.Free' x _) =
  pure $ IR.VFree x
evalElimWith _ _ (IR.Prim' p _) =
  pure $ IR.VPrim p
evalElimWith exts param (IR.App' s t _) =
  join $
    vapp param <$> evalElimWith exts param s
      <*> evalTermWith exts param t
      <*> pure ()
evalElimWith exts param (IR.Ann' _ s _ _ _) =
  evalTermWith exts param s
evalElimWith (_, eExt) _ (IR.ElimX a) =
  pure $ eExt a

evalTerm ::
  ( TC.HasThrowTC primTy primVal m,
    IR.TermX ext primTy primVal ~ Void,
    IR.ElimX ext primTy primVal ~ Void
  ) =>
  Param.Parameterisation primTy primVal ->
  IR.Term' ext primTy primVal ->
  m (IR.Value primTy primVal)
evalTerm = evalTermWith (absurd, absurd)

evalElim ::
  ( TC.HasThrowTC primTy primVal m,
    IR.TermX ext primTy primVal ~ Void,
    IR.ElimX ext primTy primVal ~ Void
  ) =>
  Param.Parameterisation primTy primVal ->
  IR.Elim' ext primTy primVal ->
  m (IR.Value primTy primVal)
evalElim = evalElimWith (absurd, absurd)
