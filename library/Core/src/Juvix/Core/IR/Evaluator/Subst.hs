{-# OPTIONS_GHC -Werror=unused-imports #-}

{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE UndecidableInstances #-}

module Juvix.Core.IR.Evaluator.Subst where

import Juvix.Core.IR.Evaluator.Weak
import qualified Juvix.Core.Application as App
import qualified Juvix.Core.IR.Types as IR
import qualified Juvix.Core.IR.Types.Base as IR
import Juvix.Library
import qualified Juvix.Library.Usage as Usage


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
