{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Module providing the `HasSubstV`-class, implementing substitution of
-- values.
module Juvix.Core.IR.Evaluator.SubstV
  ( HasSubstValue (..),
    substV,
    vapp,
  )
where

import Data.Foldable (foldr1) -- on NonEmpty
import qualified Juvix.Core.Application as App
import qualified Juvix.Core.Base.Types as Core
import Juvix.Core.IR.Evaluator.Types
import Juvix.Core.IR.Evaluator.Weak
import qualified Juvix.Core.IR.Types as IR
import qualified Juvix.Core.Parameterisation as Param
import Juvix.Library
import qualified Juvix.Library.Usage as Usage

-- | Class of values that support substitution, allows failure using @Either@.
class HasWeak a => HasSubstV extV primTy primVal a where
  substVWith ::
    -- | How many bindings have been traversed so far.
    Natural ->
    -- | Variable to substitute.
    Core.BoundVar ->
    -- | Value to substitute with.
    Core.Value' extV primTy primVal ->
    -- | Term to perform substitution on.
    a ->
    Either (Error extV extT primTy primVal) a
  default substVWith ::
    ( Generic a,
      GHasSubstV extV primTy primVal (Rep a)
    ) =>
    Natural ->
    Core.BoundVar ->
    Core.Value' extV primTy primVal ->
    a ->
    Either (Error extV extT primTy primVal) a
  substVWith b i e = fmap to . gsubstVWith b i e . from

-- | Wrapper around `substWith` for toplevel terms without free variables.
substV' ::
  HasSubstV extV primTy primVal a =>
  Core.BoundVar ->
  Core.Value' extV primTy primVal ->
  a ->
  Either (Error extV extT primTy primVal) a
substV' = substVWith 0

-- | Wrapper around `substV'` that starts at variable 0, the first bound
substV ::
  HasSubstV extV primTy primVal a =>
  Core.Value' extV primTy primVal ->
  a ->
  Either (Error extV extT primTy primVal) a
substV = substV' 0

-- | Class of terms that support substitution, resulting in a `IR.Value'`.
class HasWeak a => HasSubstValue extV primTy primVal a where
  substValueWith ::
    -- | How many bindings have been traversed so far.
    Natural ->
    -- | Variable to substitute.
    Core.BoundVar ->
    -- | Value to substitute with.
    Core.Value' extV primTy primVal ->
    a ->
    Either (Error extV extT primTy primVal) (Core.Value' extV primTy primVal)

-- | Constraint alias for values and neutrals that support substitution.
type AllSubstV extV primTy primVal =
  ( Core.ValueAll (HasSubstV extV primTy primVal) extV primTy primVal,
    Core.NeutralAll (HasSubstV extV primTy primVal) extV primTy primVal,
    HasSubstValue extV primTy primVal primTy,
    HasSubstValue extV primTy primVal primVal,
    Param.CanApply primTy,
    Param.CanApply primVal
  )

instance
  ( AllSubstV extV primTy primVal,
    Monoid (Core.XVNeutral extV primTy primVal),
    Monoid (Core.XVLam extV primTy primVal),
    Monoid (Core.XVPrimTy extV primTy primVal),
    Monoid (Core.XVPrim extV primTy primVal)
  ) =>
  HasSubstV extV primTy primVal (Core.Value' extV primTy primVal)
  where
  substVWith w i e (Core.VStar' n a) =
    Core.VStar' n <$> substVWith w i e a
  substVWith w i e (Core.VPrimTy' p _) =
    -- TODO what about the annotation?
    substValueWith w i e p
  substVWith w i e (Core.VPi' π s t a) =
    Core.VPi' π <$> substVWith w i e s
      <*> substVWith (succ w) (succ i) e t
      <*> substVWith w i e a
  substVWith w i e (Core.VLam' t a) =
    Core.VLam' <$> substVWith (succ w) (succ i) e t
      <*> substVWith w i e a
  substVWith w i e (Core.VSig' π s t a) =
    Core.VSig' π <$> substVWith w i e s
      <*> substVWith (succ w) (succ i) e t
      <*> substVWith w i e a
  substVWith w i e (Core.VPair' s t a) =
    Core.VPair' <$> substVWith w i e s
      <*> substVWith w i e t
      <*> substVWith w i e a
  substVWith w i e (Core.VUnitTy' a) =
    Core.VUnitTy' <$> substVWith w i e a
  substVWith w i e (Core.VUnit' a) =
    Core.VUnit' <$> substVWith w i e a
  substVWith w i e (Core.VNeutral' n a) =
    substNeutralWith w i e n a
  substVWith w i e (Core.VPrim' p _) =
    -- TODO what about the annotation?
    substValueWith w i e p
  substVWith w i e (Core.ValueX a) =
    Core.ValueX <$> substVWith w i e a

-- | Perform substitution on a `IR.Neutral'` and a `IR.XVNeutral`.
-- (not quite an instance of @HasSubstValue@ because of the @XVNeutral@ stuff)
substNeutralWith ::
  ( AllSubstV extV primTy primVal,
    Monoid (Core.XVNeutral extV primTy primVal),
    Monoid (Core.XVLam extV primTy primVal),
    Monoid (Core.XVPrimTy extV primTy primVal),
    Monoid (Core.XVPrim extV primTy primVal)
  ) =>
  -- | How many bindings have been traversed so far.
  Natural ->
  -- | Variable to substitute.
  Core.BoundVar ->
  -- | Value to substitute with.
  Core.Value' extV primTy primVal ->
  -- | Neutral to perform substitution on.
  Core.Neutral' extV primTy primVal ->
  -- | Extended Neutral to perform substitution on.
  Core.XVNeutral extV primTy primVal ->
  Either (Error extV extT primTy primVal) (Core.Value' extV primTy primVal)
-- not Neutral'!!!
substNeutralWith w i e (Core.NBound' j a) b = do
  a' <- substVWith w i e a
  b' <- substVWith w i e b
  pure $ case compare j i of
    LT -> Core.VNeutral' (Core.NBound' j a') b'
    EQ -> weakBy w e
    GT -> Core.VNeutral' (Core.NBound' (pred j) a') b'
substNeutralWith w i e (Core.NFree' x a) b =
  Core.VNeutral' <$> (Core.NFree' x <$> substVWith w i e a)
    <*> substVWith w i e b
substNeutralWith w i e (Core.NApp' f s a) _ =
  join $
    vapp <$> substNeutralWith w i e f mempty
      <*> substVWith w i e s
      <*> substVWith w i e a
substNeutralWith w i e (Core.NeutralX a) b =
  Core.VNeutral' <$> (Core.NeutralX <$> substVWith w i e a)
    <*> substVWith w i e b

-- | Apply two values.
vapp ::
  forall extV extT primTy primVal.
  ( AllSubstV extV primTy primVal,
    Monoid (Core.XVNeutral extV primTy primVal),
    Monoid (Core.XVLam extV primTy primVal),
    Monoid (Core.XVPrimTy extV primTy primVal),
    Monoid (Core.XVPrim extV primTy primVal)
  ) =>
  -- | Function value.
  Core.Value' extV primTy primVal ->
  -- | Argument to the function.
  Core.Value' extV primTy primVal ->
  -- | the annotation to use if the result is another application node
  -- (if it isn't, then this annotation is unused)
  Core.XNApp extV primTy primVal ->
  Either (Error extV extT primTy primVal) (Core.Value' extV primTy primVal)
vapp s t ann =
  case s of
    Core.VLam' s _ -> substV t s
    Core.VNeutral' f _ -> pure $ Core.VNeutral' (Core.NApp' f s ann) mempty
    Core.VPrimTy' p _ -> case t of
      Core.VPrimTy' q _ ->
        app' ApplyErrorT Core.VPrimTy' (\_ -> Param.pureArg) p q
      Core.VNeutral' (Core.NFree' (Core.Global y) _) _ ->
        -- TODO pattern vars also
        app' ApplyErrorT Core.VPrimTy' Param.freeArg p y
      Core.VNeutral' (Core.NBound' i _) _ ->
        app' ApplyErrorT Core.VPrimTy' Param.boundArg p i
      _ ->
        Left $ CannotApply s t NoApplyError
    Core.VPrim' p _ -> case t of
      Core.VPrim' q _ ->
        app' ApplyErrorV Core.VPrim' (\_ -> Param.pureArg) p q
      Core.VNeutral' (Core.NFree' (Core.Global y) _) _ ->
        -- TODO pattern vars also
        app' ApplyErrorV Core.VPrim' Param.freeArg p y
      Core.VNeutral' (Core.NBound' i _) _ ->
        app' ApplyErrorV Core.VPrim' Param.boundArg p i
      _ ->
        Left $ CannotApply s t NoApplyError
    _ ->
      Left $ CannotApply s t NoApplyError
  where
    app' ::
      forall ann arg fun.
      (Param.CanApply fun, Monoid ann) =>
      (Param.ApplyError fun -> ApplyError primTy primVal) ->
      (fun -> ann -> Core.Value' extV primTy primVal) ->
      (Proxy fun -> arg -> Maybe (Param.Arg fun)) ->
      fun ->
      arg ->
      Either (Error extV extT primTy primVal) (Core.Value' extV primTy primVal)
    app' err con mkArg p y =
      case mkArg Proxy y of
        Nothing -> Left $ CannotApply s t NoApplyError
        Just y ->
          Param.apply1 p y |> bimap (CannotApply s t . err) (\r -> con r mempty)

-- | Generic substitution for @f@.
class GHasWeak f => GHasSubstV extV primTy primVal f where
  gsubstVWith ::
    -- | How many bindings have been traversed so far.
    Natural ->
    -- | Variable to substitute.
    Core.BoundVar ->
    -- | Value to substitute with.
    Core.Value' extV primTy primVal ->
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

instance
  HasSubstV ext primTy primVal a =>
  HasSubstV ext primTy primVal (Param.PrimType a)

instance HasSubstV ext primTy primVal Symbol where
  substVWith _ _ _ x = pure x

instance
  ( AllSubstV extV primTy primVal,
    Monoid (Core.XVNeutral extV primTy primVal),
    Monoid (Core.XVLam extV primTy primVal),
    Monoid (Core.XVPrimTy extV primTy primVal),
    Monoid (Core.XVPrim extV primTy primVal)
  ) =>
  HasSubstValue extV primTy primVal (Core.Value' extV primTy primVal)
  where
  substValueWith = substVWith

instance
  ( AllSubstV ext primTy primVal,
    Monoid (Core.XNBound ext primTy primVal),
    Monoid (Core.XNFree ext primTy primVal),
    Monoid (Core.XVNeutral ext primTy primVal),
    Monoid (Core.XVLam ext primTy primVal),
    Monoid (Core.XVPrimTy ext primTy primVal),
    Monoid (Core.XVPrim ext primTy primVal)
  ) =>
  HasSubstValue ext primTy primVal App.DeBruijn
  where
  substValueWith b i e (App.BoundVar j) =
    substNeutralWith b i e (Core.NBound' j mempty) mempty
  substValueWith _ _ _ (App.FreeVar x) =
    pure $ Core.VNeutral' (Core.NFree' (Core.Global x) mempty) mempty

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
    Core.ValueAll HasWeak ext primTy primVal,
    Core.NeutralAll HasWeak ext primTy primVal,
    HasWeak primTy,
    HasWeak primVal,
    Monoid (Core.XVPi ext primTy primVal)
  ) =>
  HasSubstValue ext primTy primVal (NonEmpty a)
  where
  substValueWith b i e tys =
    foldr1 pi <$> traverse (substValueWith b i e) tys
    where
      pi s t = Core.VPi' Usage.Omega s (weak t) mempty

-- TODO generalise @IR.T@
instance
  ( HasWeak primTy,
    HasWeak primVal,
    HasSubstValue IR.T primTy (Param.TypedPrim primTy primVal) primTy,
    Param.CanApply primTy,
    Param.CanApply (Param.TypedPrim primTy primVal)
  ) =>
  HasSubstValue
    IR.T
    primTy
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

-- | Transform an `App.Arg` into a `IR.Value`.
argToValue ::
  App.Arg (Param.PrimType primTy) primVal ->
  IR.Value primTy (Param.TypedPrim primTy primVal)
argToValue = \case
  App.TermArg (App.Take {type', term}) ->
    IR.VPrim $ App.Return {retType = type', retTerm = term}
  App.BoundArg i -> Core.VBound i
  App.FreeArg x -> Core.VFree $ Core.Global x
