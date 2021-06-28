{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides weakening / shifting of de Bruijn indices, that is the
-- renumbering of free variables in terms.
module Juvix.Core.IR.Evaluator.Weak where

import qualified Juvix.Core.Application as App
import qualified Juvix.Core.IR.Types as IR
import qualified Juvix.Core.IR.Types.Base as IR
import qualified Juvix.Core.Parameterisation as Param
import Juvix.Library
import qualified Juvix.Library.Usage as Usage

-- | Weakening / shifting of de Bruijn terms.
class HasWeak a where
  weakBy' ::
    -- | Amount to shift.
    Natural ->
    -- | Cutoff value for shifting.
    IR.BoundVar ->
    -- | Term to apply weakening to.
    a ->
    a
  default weakBy' ::
    (Generic a, GHasWeak (Rep a)) =>
    Natural ->
    IR.BoundVar ->
    a ->
    a
  weakBy' b i = to . gweakBy' b i . from

-- | Perform weakening on a term, starting with the closest binder.
weakBy :: HasWeak a => Natural -> a -> a
weakBy b = weakBy' b 0

-- | Perform weakening on term, shifting a single binding with relation to a
-- given bound variable.
weak' :: HasWeak a => IR.BoundVar -> a -> a
weak' = weakBy' 1

-- | Perform weakening on a toplevel term, shifting a single binding with
-- relation to a given bound variable.
weak :: HasWeak a => a -> a
weak = weak' 0

-- | Constraint alias for terms and eliminations that can be weakened,
type AllWeak ext primTy primVal =
  ( HasWeak primTy,
    HasWeak primVal,
    IR.TermAll HasWeak ext primTy primVal,
    IR.ElimAll HasWeak ext primTy primVal
  )

-- | Weakening implementation for terms.
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

-- | Weakening implementation for eliminations.
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

-- | Constraint alias for valaues that can be weakened,
type AllWeakV ext primTy primVal =
  ( HasWeak primTy,
    HasWeak primVal,
    IR.ValueAll HasWeak ext primTy primVal,
    IR.NeutralAll HasWeak ext primTy primVal
  )

-- | Weakening implementation for values.
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

-- | Weakening implementation for neutral values.
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

-- | Generic weakening / shifting (renumbering of free variables) of de
-- Bruijn terms.
class GHasWeak f where
  gweakBy' ::
    -- | Amount to shift.
    Natural ->
    -- | Cutoff value for shifting.
    IR.BoundVar ->
    -- | Term to apply weakening to.
    f t ->
    f t

instance GHasWeak U1 where gweakBy' _ _ U1 = U1

instance GHasWeak V1 where gweakBy' _ _ v = case v of -- TODO

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

instance HasWeak a => HasWeak (Param.PrimType a)

instance HasWeak Symbol where weakBy' _ _ x = x

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
