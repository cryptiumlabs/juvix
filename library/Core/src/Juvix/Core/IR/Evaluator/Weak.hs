{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE UndecidableInstances #-}

module Juvix.Core.IR.Evaluator.Weak where

import qualified Juvix.Core.Application as App
import qualified Juvix.Core.Base.Types as Core
import qualified Juvix.Core.Parameterisation as Param
import Juvix.Library
import qualified Juvix.Library.Usage as Usage

class HasWeak a where
  weakBy' :: Natural -> Core.BoundVar -> a -> a
  default weakBy' ::
    (Generic a, GHasWeak (Rep a)) =>
    Natural ->
    Core.BoundVar ->
    a ->
    a
  weakBy' b i = to . gweakBy' b i . from

weakBy :: HasWeak a => Natural -> a -> a
weakBy b = weakBy' b 0

weak' :: HasWeak a => Core.BoundVar -> a -> a
weak' = weakBy' 1

weak :: HasWeak a => a -> a
weak = weak' 0

type AllWeak ext primTy primVal =
  ( HasWeak primTy,
    HasWeak primVal,
    Core.TermAll HasWeak ext primTy primVal,
    Core.ElimAll HasWeak ext primTy primVal
  )

instance AllWeak ext primTy primVal => HasWeak (Core.Term' ext primTy primVal) where
  weakBy' b i (Core.Star' u a) =
    Core.Star' u (weakBy' b i a)
  weakBy' b i (Core.PrimTy' p a) =
    Core.PrimTy' (weakBy' b i p) (weakBy' b i a)
  weakBy' b i (Core.Prim' p a) =
    Core.Prim' (weakBy' b i p) (weakBy' b i a)
  weakBy' b i (Core.Pi' π s t a) =
    Core.Pi' π (weakBy' b i s) (weakBy' b (succ i) t) (weakBy' b i a)
  weakBy' b i (Core.Lam' t a) =
    Core.Lam' (weakBy' b (succ i) t) (weakBy' b i a)
  weakBy' b i (Core.Sig' π s t a) =
    Core.Sig' π (weakBy' b i s) (weakBy' b (succ i) t) (weakBy' b i a)
  weakBy' b i (Core.Pair' s t a) =
    Core.Pair' (weakBy' b i s) (weakBy' b i t) (weakBy' b i a)
  weakBy' b i (Core.UnitTy' a) =
    Core.UnitTy' (weakBy' b i a)
  weakBy' b i (Core.Unit' a) =
    Core.Unit' (weakBy' b i a)
  weakBy' b i (Core.Let' π s t a) =
    Core.Let' π (weakBy' b i s) (weakBy' b (succ i) t) (weakBy' b i a)
  weakBy' b i (Core.Elim' f a) =
    Core.Elim' (weakBy' b i f) (weakBy' b i a)
  weakBy' b i (Core.TermX a) =
    Core.TermX (weakBy' b i a)

instance AllWeak ext primTy primVal => HasWeak (Core.Elim' ext primTy primVal) where
  weakBy' b i (Core.Bound' j a)
    | j >= i = Core.Bound' (j + b) a'
    | otherwise = Core.Bound' j a'
    where
      a' = weakBy' b i a
  weakBy' b i (Core.Free' x a) =
    Core.Free' x (weakBy' b i a)
  weakBy' b i (Core.App' s t a) =
    Core.App' (weakBy' b i s) (weakBy' b i t) (weakBy' b i a)
  weakBy' b i (Core.Ann' π s t l a) =
    Core.Ann' π (weakBy' b i s) (weakBy' b i t) l (weakBy' b i a)
  weakBy' b i (Core.ElimX a) =
    Core.ElimX (weakBy' b i a)

type AllWeakV ext primTy primVal =
  ( HasWeak primTy,
    HasWeak primVal,
    Core.ValueAll HasWeak ext primTy primVal,
    Core.NeutralAll HasWeak ext primTy primVal
  )

instance
  AllWeakV ext primTy primVal =>
  HasWeak (Core.Value' ext primTy primVal)
  where
  weakBy' b i (Core.VStar' n a) =
    Core.VStar' n (weakBy' b i a)
  weakBy' b i (Core.VPrimTy' p a) =
    Core.VPrimTy' (weakBy' b i p) (weakBy' b i a)
  weakBy' b i (Core.VPi' π s t a) =
    Core.VPi' π (weakBy' b i s) (weakBy' b (succ i) t) (weakBy' b i a)
  weakBy' b i (Core.VLam' t a) =
    Core.VLam' (weakBy' b (succ i) t) (weakBy' b i a)
  weakBy' b i (Core.VSig' π s t a) =
    Core.VSig' π (weakBy' b i s) (weakBy' b (succ i) t) (weakBy' b i a)
  weakBy' b i (Core.VPair' s t a) =
    Core.VPair' (weakBy' b i s) (weakBy' b (succ i) t) (weakBy' b i a)
  weakBy' b i (Core.VUnitTy' a) =
    Core.VUnitTy' (weakBy' b i a)
  weakBy' b i (Core.VUnit' a) =
    Core.VUnit' (weakBy' b i a)
  weakBy' b i (Core.VNeutral' n a) =
    Core.VNeutral' (weakBy' b i n) (weakBy' b i a)
  weakBy' b i (Core.VPrim' p a) =
    Core.VPrim' (weakBy' b i p) (weakBy' b i a)
  weakBy' b i (Core.ValueX a) =
    Core.ValueX (weakBy' b i a)

instance
  AllWeakV ext primTy primVal =>
  HasWeak (Core.Neutral' ext primTy primVal)
  where
  weakBy' b i (Core.NBound' j a)
    | j >= i = Core.NBound' (j + b) a'
    | otherwise = Core.NBound' j a'
    where
      a' = weakBy' b i a
  weakBy' b i (Core.NFree' x a) =
    Core.NFree' x (weakBy' b i a)
  weakBy' b i (Core.NApp' f s a) =
    Core.NApp' (weakBy' b i f) (weakBy' b i s) (weakBy' b i a)
  weakBy' b i (Core.NeutralX a) =
    Core.NeutralX (weakBy' b i a)

class GHasWeak f where
  gweakBy' :: Natural -> Core.BoundVar -> f t -> f t

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
