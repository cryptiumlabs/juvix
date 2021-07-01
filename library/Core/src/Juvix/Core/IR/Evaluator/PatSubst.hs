{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE UndecidableInstances #-}

module Juvix.Core.IR.Evaluator.PatSubst where

import Data.Foldable (foldr1) -- on NonEmpty
import qualified Data.IntMap as IntMap
import qualified Juvix.Core.Application as App
import qualified Juvix.Core.Base.TransformExt.OnlyExts as OnlyExts
import qualified Juvix.Core.Base.Types as Core
import Juvix.Core.IR.Evaluator.Weak
import qualified Juvix.Core.IR.Types as IR
import qualified Juvix.Core.Parameterisation as Param
import Juvix.Library
import qualified Juvix.Library.Usage as Usage

class HasWeak a => HasPatSubst extT primTy primVal a where
  -- returns either a substituted term or an unbound pattern var
  -- TODO: use @validation@ to return all unbound vars
  patSubst' ::
    -- | How many bindings have been traversed so far
    Natural ->
    -- | Mapping of pattern variables to matched subterms
    Core.PatternMap (Core.Elim' extT primTy primVal) ->
    a ->
    Either Core.PatternVar a
  default patSubst' ::
    ( Generic a,
      GHasPatSubst extT primTy primVal (Rep a)
    ) =>
    Natural ->
    Core.PatternMap (Core.Elim' extT primTy primVal) ->
    a ->
    Either Core.PatternVar a
  patSubst' b m = fmap to . gpatSubst' b m . from

patSubst ::
  (HasPatSubst extT primTy primVal a) =>
  Core.PatternMap (Core.Elim' extT primTy primVal) ->
  a ->
  Either Core.PatternVar a
patSubst = patSubst' 0

class HasWeak a => HasPatSubstTerm extT primTy primVal a where
  -- returns either a substituted term or an unbound pattern var
  -- TODO: use @validation@ to return all unbound vars
  patSubstTerm' ::
    -- | How many bindings have been traversed so far
    Natural ->
    -- | Mapping of pattern variables to matched subterms
    Core.PatternMap (Core.Elim' extT primTy primVal) ->
    a ->
    Either Core.PatternVar (Core.Term' extT primTy primVal)

patSubstTerm ::
  (HasPatSubstTerm extT primTy primVal a) =>
  Core.PatternMap (Core.Elim' extT primTy primVal) ->
  a ->
  Either Core.PatternVar (Core.Term' extT primTy primVal)
patSubstTerm = patSubstTerm' 0

type AllPatSubst ext primTy primVal =
  ( Core.TermAll (HasPatSubst ext primTy primVal) ext primTy primVal,
    Core.ElimAll (HasPatSubst ext primTy primVal) ext primTy primVal,
    HasPatSubstTerm ext primTy primVal primTy,
    HasPatSubstTerm ext primTy primVal primVal
  )

instance
  AllPatSubst ext primTy primVal =>
  HasPatSubst ext primTy primVal (Core.Term' ext primTy primVal)
  where
  patSubst' b m (Core.Star' u a) =
    Core.Star' u <$> patSubst' b m a
  patSubst' b m (Core.PrimTy' t _) =
    -- FIXME annotation?
    patSubstTerm' b m t
  patSubst' b m (Core.Prim' p _) =
    -- FIXME annotation?
    patSubstTerm' b m p
  patSubst' b m (Core.Pi' π s t a) =
    Core.Pi' π <$> patSubst' b m s
      <*> patSubst' (succ b) m t
      <*> patSubst' b m a
  patSubst' b m (Core.Lam' t a) =
    Core.Lam' <$> patSubst' (succ b) m t
      <*> patSubst' b m a
  patSubst' b m (Core.Sig' π s t a) =
    Core.Sig' π <$> patSubst' b m s
      <*> patSubst' (succ b) m t
      <*> patSubst' b m a
  patSubst' b m (Core.Pair' s t a) =
    Core.Pair' <$> patSubst' b m s
      <*> patSubst' b m t
      <*> patSubst' b m a
  patSubst' b m (Core.UnitTy' a) =
    Core.UnitTy' <$> patSubst' b m a
  patSubst' b m (Core.Unit' a) =
    Core.Unit' <$> patSubst' b m a
  patSubst' b m (Core.Let' π l t a) =
    Core.Let' π <$> patSubst' b m l
      <*> patSubst' (succ b) m t
      <*> patSubst' b m a
  patSubst' b m (Core.Elim' e a) =
    Core.Elim' <$> patSubst' b m e
      <*> patSubst' b m a
  patSubst' b m (Core.TermX a) =
    Core.TermX <$> patSubst' b m a

instance
  AllPatSubst ext primTy primVal =>
  HasPatSubst ext primTy primVal (Core.Elim' ext primTy primVal)
  where
  patSubst' b m (Core.Bound' j a) =
    Core.Bound' j <$> patSubst' b m a
  patSubst' b m (Core.Free' (Core.Pattern x) _) =
    case IntMap.lookup x m of
      Nothing -> Left x
      Just e -> pure $ weakBy b e
  patSubst' b m (Core.Free' x a) =
    Core.Free' x <$> patSubst' b m a
  patSubst' b m (Core.App' f e a) =
    Core.App' <$> patSubst' b m f
      <*> patSubst' b m e
      <*> patSubst' b m a
  patSubst' b m (Core.Ann' π s t ℓ a) =
    Core.Ann' π <$> patSubst' b m s
      <*> patSubst' b m t
      <*> pure ℓ
      <*> patSubst' b m a
  patSubst' b m (Core.ElimX a) =
    Core.ElimX <$> patSubst' b m a

class GHasWeak f => GHasPatSubst extT primTy primVal f where
  gpatSubst' ::
    -- | How many bindings have been traversed so far
    Natural ->
    -- | Mapping of pattern variables to matched subterms
    Core.PatternMap (Core.Elim' extT primTy primVal) ->
    f t ->
    Either Core.PatternVar (f t)

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
  ( HasWeak primTy,
    HasWeak primVal
  ) =>
  HasPatSubstTerm
    (OnlyExts.T ext)
    primTy
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
  Core.Elim' (OnlyExts.T ext) primTy (Param.TypedPrim primTy primVal)
takeToElim (App.Take {type', term}) =
  let term' = IR.Prim (App.Return {retType = type', retTerm = term})
      ty' = typeToTerm type'
   in IR.Ann Usage.Omega term' ty' 0

argToTerm ::
  App.Arg (Param.PrimType primTy) primVal ->
  Core.Term' (OnlyExts.T ext) primTy (Param.TypedPrim primTy primVal)
argToTerm = \case
  App.TermArg (App.Take {type', term}) ->
    IR.Prim $ App.Return {retType = type', retTerm = term}
  App.BoundArg i -> IR.Elim $ IR.Bound i
  App.FreeArg x -> IR.Elim $ IR.Free $ Core.Global x

typeToTerm ::
  ( Monoid (Core.XPi ext primTy primVal),
    Monoid (Core.XPrimTy ext primTy primVal)
  ) =>
  Param.PrimType primTy ->
  Core.Term' ext primTy primVal
typeToTerm tys = foldr1 arr $ map prim tys
  where
    prim ty = Core.PrimTy' ty mempty
    arr s t = Core.Pi' Usage.Omega s t mempty
