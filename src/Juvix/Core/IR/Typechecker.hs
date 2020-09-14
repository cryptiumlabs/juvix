module Juvix.Core.IR.Typechecker
  ( module Juvix.Core.IR.Typechecker,
    module Typed,
    module Env,
  )
where

import Data.Foldable (foldr1)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.IntMap.Strict as IntMap
import Data.List.NonEmpty ((<|))
import qualified Juvix.Core.IR.Evaluator as Eval
import Juvix.Core.IR.Typechecker.Env as Env
import Juvix.Core.IR.Typechecker.Types as Typed
import qualified Juvix.Core.IR.Types as IR
import qualified Juvix.Core.Parameterisation as Param
import qualified Juvix.Core.Usage as Usage
import Juvix.Library hiding (Datatype)

data Leftovers primTy primVal a
  = Leftovers
      { loValue :: a,
        loLocals :: UContext primTy primVal,
        loPatVars :: PatUsages primTy primVal
      }
  deriving (Eq, Show, Generic)

leftoversOk :: Leftovers primTy primVal a -> Bool
leftoversOk (Leftovers {loLocals, loPatVars}) =
  all leftoverOk loLocals && all leftoverOk loPatVars

leftoverOk :: Usage.T -> Bool
leftoverOk ρ = ρ == Usage.Omega || ρ == mempty

-- | Checks a 'Term' against an annotation and returns a decorated term if
-- successful.
typeTerm ::
  (Eq primTy, Eq primVal) =>
  Param.Parameterisation primTy primVal ->
  IR.Term primTy primVal ->
  Annotation primTy primVal ->
  EnvTypecheck primTy primVal (Typed.Term primTy primVal)
typeTerm param t ann = loValue <$> typeTermWith param IntMap.empty [] t ann

typeTermWith ::
  (Eq primTy, Eq primVal) =>
  Param.Parameterisation primTy primVal ->
  PatBinds primTy primVal ->
  Context primTy primVal ->
  IR.Term primTy primVal ->
  Annotation primTy primVal ->
  EnvTypecheck primTy primVal
    (Leftovers primTy primVal (Typed.Term primTy primVal))
typeTermWith param pats ctx t ann =
  execInner (withLeftovers $ typeTerm' t ann) (InnerState param pats ctx)

-- | Infers the type and usage for an 'Elim' and returns it decorated with this
-- information.
typeElim ::
  (Eq primTy, Eq primVal) =>
  Param.Parameterisation primTy primVal ->
  IR.Elim primTy primVal ->
  Usage.T ->
  EnvTypecheck primTy primVal (Typed.Elim primTy primVal)
typeElim param e σ =
  loValue <$> typeElimWith param IntMap.empty [] e σ

typeElimWith ::
  (Eq primTy, Eq primVal) =>
  Param.Parameterisation primTy primVal ->
  PatBinds primTy primVal ->
  Context primTy primVal ->
  IR.Elim primTy primVal ->
  Usage.T ->
  EnvTypecheck primTy primVal
    (Leftovers primTy primVal (Typed.Elim primTy primVal))
typeElimWith param pats ctx e σ =
  execInner (withLeftovers $ typeElim' e σ) (InnerState param pats ctx)

withLeftovers ::
  InnerTC primTy primVal a ->
  InnerTC primTy primVal (Leftovers primTy primVal a)
withLeftovers m =
  Leftovers <$> m
    <*> fmap (fmap annUsage) (get @"bound")
    <*> fmap (fmap annUsage) (get @"patBinds")

typeTerm' ::
  (Eq primTy, Eq primVal) =>
  IR.Term primTy primVal ->
  Annotation primTy primVal ->
  InnerTC primTy primVal (Typed.Term primTy primVal)
typeTerm' term ann@(Annotation σ ty) =
  case term of
    IR.Star i -> do
      requireZero σ
      j <- requireStar ty
      requireUniverseLT i j
      pure $ Typed.Star i ann
    IR.PrimTy t -> do
      requireZero σ
      void $ requireStar ty
      pure $ Typed.PrimTy t ann
    IR.Prim p -> do
      requirePrimType p ty
      pure $ Typed.Prim p $ Annotation σ ty
    IR.Pi π a b -> do
      requireZero σ
      void $ requireStar ty
      a' <- typeTerm' a ann
      b' <- typeTerm' b ann
      pure $ Typed.Pi π a' b' ann
    IR.Lam t -> do
      (π, a, b) <- requirePi ty
      let varAnn = Annotation (σ <.> π) a
          tAnn = Annotation σ b
      t' <- withLocal varAnn $ typeTerm' t tAnn
      let anns = BindAnnotation {baBindAnn = varAnn, baResAnn = ann}
      pure $ Typed.Lam t' anns
    IR.Let σb b t -> do
      b' <- typeElim' b σb
      let bAnn = getElimAnn b'
          tAnn = Annotation σ (Eval.weak ty)
      t' <- withLocal bAnn $ typeTerm' t tAnn
      let anns = BindAnnotation {baBindAnn = bAnn, baResAnn = ann}
      pure $ Typed.Let σb b' t' anns
    IR.Elim e -> do
      e' <- typeElim' e σ
      let ty' = annType $ getElimAnn e'
      requireSubtype e ty ty'
      pure $ Typed.Elim e' ann

typeElim' ::
  (Eq primTy, Eq primVal) =>
  IR.Elim primTy primVal ->
  Usage.T ->
  InnerTC primTy primVal (Typed.Elim primTy primVal)
typeElim' elim σ =
  case elim of
    IR.Bound i -> do
      ty <- useLocal σ i
      pure $ Typed.Bound i $ Annotation σ ty
    IR.Free px@(IR.Pattern x) -> do
      ty <- usePatVar σ x
      pure $ Typed.Free px $ Annotation σ ty
    IR.Free gx@(IR.Global x) -> do
      (ty, π') <- lookupGlobal x
      when (π' == IR.GZero) $ requireZero σ
      pure $ Typed.Free gx $ Annotation σ ty
    IR.App s t -> do
      s' <- typeElim' s σ
      (π, a, b) <- requirePi $ annType $ getElimAnn s'
      let tAnn = Annotation (σ <.> π) a
      t' <- typeTerm' t tAnn
      ty <- substApp b t
      pure $ Typed.App s' t' $ Annotation σ ty
    IR.Ann π s a ℓ -> do
      a' <- typeTerm' a $ Annotation mempty (IR.VStar ℓ)
      ty <- evalTC a
      let ann = Annotation σ ty
      s' <- typeTerm' s ann
      pure $ Typed.Ann π s' a' ℓ ann

pushLocal :: Annotation primTy primVal -> InnerTC primTy primVal ()
pushLocal ann = modify @"bound" (ann :)

popLocal :: InnerTC primTy primVal ()
popLocal = do
  ctx <- get @"bound"
  case ctx of
    Annotation ρ _ : ctx -> do
      unless (leftoverOk ρ) $ throwTC (LeftoverUsage ρ)
      put @"bound" ctx
    [] -> do
      throwTC (UnboundIndex 0)

withLocal ::
  Annotation primTy primVal ->
  InnerTC primTy primVal a ->
  InnerTC primTy primVal a
withLocal ann m = pushLocal ann *> m <* popLocal

requireZero :: Usage.T -> InnerTC primTy primVal ()
requireZero π = unless (π == mempty) $ throwTC (UsageMustBeZero π)

requireStar :: IR.Value primTy primVal -> InnerTC primTy primVal IR.Universe
requireStar (IR.VStar j) = pure j
requireStar ty = throwTC (ShouldBeStar ty)

requireUniverseLT :: IR.Universe -> IR.Universe -> InnerTC primTy primVal ()
requireUniverseLT i j = unless (i < j) $ throwTC (UniverseMismatch i j)

requirePrimType ::
  primVal ->
  IR.Value primTy primVal ->
  InnerTC primTy primVal ()
requirePrimType p ty = do
  param <- ask @"param"
  ty' <- toPrimTy ty
  unless (Param.hasType param p ty') $
    throwTC (WrongPrimTy p ty')

toPrimTy :: IR.Value primTy primVal -> InnerTC primTy primVal (NonEmpty primTy)
toPrimTy ty = maybe (throwTC $ NotPrimTy ty) pure $ go ty
  where
    go (IR.VPrimTy t) = pure $ t :| []
    go (IR.VPi _ (IR.VPrimTy s) t) = (s <|) <$> go t
    go _ = empty

type PiParts primTy primVal =
  (Usage.T, IR.Value primTy primVal, IR.Value primTy primVal)

requirePi ::
  IR.Value primTy primVal ->
  InnerTC primTy primVal (PiParts primTy primVal)
requirePi (IR.VPi π a b) = pure (π, a, b)
requirePi ty = throwTC (ShouldBeFunctionType ty)

requireSubtype ::
  (Eq primTy, Eq primVal) =>
  IR.Elim primTy primVal ->
  IR.Value primTy primVal ->
  IR.Value primTy primVal ->
  InnerTC primTy primVal ()
requireSubtype subj exp got =
  unless (got <: exp) $ throwTC (TypeMismatch subj exp got)

useLocal ::
  Usage.T ->
  IR.BoundVar ->
  InnerTC primTy primVal (IR.Value primTy primVal)
useLocal π var = do
  ctx <- get @"bound"
  (ty, ctx) <- go 1 var ctx
  put @"bound" ctx
  pure ty
  where
    go _ _ [] = throwTC (UnboundIndex var)
    go w 0 (Annotation ρ ty : ctx) = do
      case ρ `Usage.minus` π of
        Just ρ' -> pure (Eval.weakBy w ty, Annotation ρ' ty : ctx)
        Nothing -> throwTC (InsufficientUsage π ρ)
    go w i (b : ctx) = second (b :) <$> go (w + 1) (i - 1) ctx

usePatVar ::
  Usage.T ->
  IR.PatternVar ->
  InnerTC primTy primVal (IR.Value primTy primVal)
usePatVar π var = do
  -- TODO a single traversal with alterF or something
  mAnn <- gets @"patBinds" $ IntMap.lookup var
  case mAnn of
    Just (Annotation ρ ty)
      | Just ρ' <- ρ `Usage.minus` π -> do
        modify @"patBinds" $ IntMap.insert var $ Annotation ρ' ty
        pure ty
      | otherwise -> do
        throwTC (InsufficientUsage π ρ)
    Nothing -> do
      throwTC (UnboundPatVar var)

lookupGlobal ::
  IR.GlobalName ->
  InnerTC primTy primVal (IR.Value primTy primVal, IR.GlobalUsage)
lookupGlobal x = do
  mdefn <- asks @"globals" $ HashMap.lookup x
  case mdefn of
    Just defn -> pure $ makeGAnn defn
    Nothing -> throwTC (UnboundGlobal x)
  where
    makeGAnn (GDatatype (IR.Datatype {dataArgs, dataLevel})) =
      (foldr makePi (IR.VStar dataLevel) dataArgs, IR.GZero)
    makeGAnn (GDataCon (IR.DataCon {conType})) =
      (conType, IR.GOmega)
    makeGAnn (GFunction (IR.Function {funType, funUsage})) =
      (funType, funUsage)
    makeGAnn (GAbstract absUsage absType) =
      (absType, absUsage)
    makePi (IR.DataArg {argUsage, argType}) res = IR.VPi argUsage argType res

substApp ::
  IR.Value primTy primVal ->
  IR.Term primTy primVal ->
  InnerTC primTy primVal (IR.Value primTy primVal)
substApp ty arg = do
  arg' <- evalTC arg
  param <- ask @"param"
  Eval.substV param arg' ty

evalTC ::
  IR.Term primTy primVal ->
  InnerTC primTy primVal (IR.Value primTy primVal)
evalTC t = do
  param <- ask @"param"
  Eval.evalTerm param t

-- | Subtyping. If @s <: t@ then @s@ is a subtype of @t@, i.e. everything of
-- type @s@ can also be checked against type @t@.
--
-- Currently subtyping consists of the following:
--
-- * Consistency of universe levels (@*ᵢ <: *ⱼ@ if @i ≤ j@)
-- * Usage compatibility (@(π x: A) → B <: (ω x: A) → B@ for finite @π@)
-- * Contravariant domain & covariant codomain
--   (@(π x: A₁) → B₁ <: (π x: A₂) → B₂@ if
--    @A₂ <: A₁@ and @B₁ <: B₂@)
-- * It doesn't descend into any other structures
--   (TODO: which ones are safe to do so?)
(<:) ::
  (Eq primTy, Eq primVal) =>
  IR.Value primTy primVal ->
  IR.Value primTy primVal ->
  Bool
IR.VStar i <: IR.VStar j = i <= j
IR.VPi π1 s1 t1 <: IR.VPi π2 s2 t2 =
  π2 `Usage.allows` π1 && s2 <: s1 && t1 <: t2
s1 <: s2 = s1 == s2

infix 4 <: -- same as (<), etc
