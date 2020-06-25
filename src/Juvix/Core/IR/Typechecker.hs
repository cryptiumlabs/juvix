{-# OPTIONS_GHC -fdefer-typed-holes #-}
module Juvix.Core.IR.Typechecker
  ( module Juvix.Core.IR.Typechecker,
    -- * reexports from ….Types
    T,
    Annotation' (..),
    Annotation,
    TypecheckError' (..),
    TypecheckError,
    getTermAnn,
    getElimAnn,
    -- * reexports from ….Env
    EnvCtx (..),
    EnvAlias,
    EnvTypecheck (..),
    exec,
    Globals,
    Global (..),
    Context,
    TContext,
    lookupT,
    UContext,
    lookupU,
  )
where

import qualified Data.HashMap.Strict as HashMap
import qualified Juvix.Core.IR.Evaluator as Eval
import Juvix.Core.IR.Typechecker.Env
import Juvix.Core.IR.Typechecker.Types as Typed
import qualified Juvix.Core.IR.Types as IR
import qualified Juvix.Core.Parameterisation as Param
import qualified Juvix.Core.Usage as Usage
import Juvix.Library hiding (Datatype)
import qualified Data.IntMap as IntMap
import Data.Foldable (foldr1)


-- | Checks a 'Term' against an annotation and returns a decorated term if
-- successful.
typeTerm ::
  (Eq primTy, Eq primVal) =>
  Param.Parameterisation primTy primVal ->
  IR.Term primTy primVal ->
  Annotation primTy primVal ->
  EnvTypecheck primTy primVal (Typed.Term primTy primVal)
typeTerm param t ann =
  execInner (typeTerm' t ann) (InnerState param mempty [] [])
  -- FIXME: pattern vars                        ↑

-- | Infers the type and usage for an 'Elim' and returns it decorated with this
-- information.
typeElim ::
  (Eq primTy, Eq primVal) =>
  Param.Parameterisation primTy primVal ->
  IR.Elim primTy primVal ->
  Usage.T ->
  EnvTypecheck primTy primVal (Typed.Elim primTy primVal)
typeElim param e σ =
  execInner (typeElim' e σ) (InnerState param mempty [] [])
  -- FIXME: pattern vars                    ↑


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

    IR.Pi π a b -> do
      requireZero σ
      void $ requireStar ty
      a' <- typeTerm' a ann
      b' <- typeTerm' b ann
      pure $ Typed.Pi π a' b' ann

    IR.Lam t -> do
      (π, a, b) <- requirePi ty
      let varAnn = Annotation (σ <.> π) a
          tAnn   = Annotation σ b
      t' <- withLocal varAnn $ typeTerm' t tAnn
      pure $ Typed.Lam t' ann

    IR.Let σb b t -> do
      b' <- typeElim' b σb
      let bAnn = getElimAnn b'
          tAnn = Annotation σ (Eval.weakValue ty)
      t' <- withLocal bAnn $ typeTerm' t tAnn
      pure $ Typed.Let σb b' t' ann

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
      useLocal σ i
      ty <- localType i
      pure $ Typed.Bound i $ Annotation σ ty

    IR.Free (IR.Pattern x) -> do
      _
      -- TODO what to do about usages of pat vars

    IR.Free gx@(IR.Global x) -> do
      (ty, π') <- lookupGlobal x
      when (π' == GZero) $ requireZero σ
      pure $ Typed.Free gx $ Annotation σ ty

    IR.Prim p -> do
      ty <- primType p
      pure $ Typed.Prim p $ Annotation σ ty

    IR.App s t -> do
      s' <- typeElim' s σ
      (π, a, b) <- requirePi $ annType $ getElimAnn s'
      let tAnn   = Annotation (σ <.> π) a
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
pushLocal (Annotation π ty) = do
  modify @"boundTypes" (ty :)
  modify @"boundUsages" (π :)

popLocal :: InnerTC primTy primVal ()
popLocal = do
  tctx <- get @"boundTypes"
  uctx <- get @"boundUsages"
  case (tctx, uctx) of
    (_:tctx, π:uctx) -> do
      unless (π == mempty || π == Usage.Omega) $
        throwTC _
      put @"boundTypes" tctx
      put @"boundUsages" uctx
    _ ->
      throwTC _

withLocal :: Annotation primTy primVal
          -> InnerTC primTy primVal a
          -> InnerTC primTy primVal a
withLocal ann m = pushLocal ann *> m <* popLocal


requireZero :: Usage.T -> InnerTC primTy primVal ()
requireZero π = unless (π == mempty) $ throwTC SigmaMustBeZero

requireStar :: IR.Value primTy primVal -> InnerTC primTy primVal IR.Universe
requireStar (IR.VStar j) = pure j
requireStar ty = throwTC $ ShouldBeStar ty

requireUniverseLT :: IR.Universe -> IR.Universe -> InnerTC primTy primVal ()
requireUniverseLT i j = unless (i < j) $ throwTC $ UniverseMismatch i j

type PiParts primTy primVal =
  (Usage.T, IR.Value primTy primVal, IR.Value primTy primVal)

requirePi :: IR.Value primTy primVal
          -> InnerTC primTy primVal (PiParts primTy primVal)
requirePi (IR.VPi π a b) = pure (π, a, b)
requirePi ty = throwTC $ ShouldBeFunctionType ty

requireSubtype :: (Eq primTy, Eq primVal)
               => IR.Elim primTy primVal
               -> IR.Value primTy primVal
               -> IR.Value primTy primVal
               -> InnerTC primTy primVal ()
requireSubtype e s t = unless (s <: t) $ throwTC $ TypeMismatch e s t


useLocal :: Usage.T -> IR.BoundVar -> InnerTC primTy primVal ()
useLocal π var = do
  uctx <- get @"boundUsages"
  uctx' <- go var uctx
  put @"boundUsages" uctx'
 where
  go _ [] = throwTC $ UnboundIndex var
  go 0 (ρ:ρs) = do
    case ρ `Usage.minus` π of
      Just ρ' -> pure $ ρ' : ρs
      Nothing -> throwTC _
  go i (ρ:ρs) = (ρ:) <$> go (i - 1) ρs

localType :: IR.BoundVar -> InnerTC primTy primVal (IR.Value primTy primVal)
localType var = do
  tctx <- get @"boundTypes"
  case lookupT tctx var of
    Just ty -> pure ty
    Nothing -> throwTC $ UnboundIndex var


data GlobalUsage = GZero | GOmega deriving (Eq, Ord, Show, Bounded, Enum)

lookupGlobal :: IR.GlobalName
             -> InnerTC primTy primVal (IR.Value primTy primVal, GlobalUsage)
lookupGlobal x = do
  mdefn <- asks @"globals" $ HashMap.lookup x
  case mdefn of
    Just defn -> pure $ makeGAnn defn
    Nothing   -> throwTC $ GlobalNotInScope x
  where
    makeGAnn (GDatatype (IR.Datatype {dataArgs, dataLevel})) =
      (foldr makePi (IR.VStar dataLevel) dataArgs, GZero)
    makeGAnn (GDataCon (IR.DataCon {conType})) =
      (conType, GOmega)
    makeGAnn (GFunction (IR.Function {funType})) =
      (funType, GOmega)

    makePi (IR.DataArg {argUsage, argType}) res = IR.VPi argUsage argType res


primType :: primVal -> InnerTC primTy primVal (IR.Value primTy primVal)
primType v = do
  asks @"param" $ \param ->
    Param.typeOf param v
      |> fmap IR.VPrimTy
      |> foldr1 (\s t -> IR.VPi Usage.Omega s t)


substApp :: IR.Value primTy primVal
         -> IR.Term  primTy primVal
         -> InnerTC primTy primVal (IR.Value primTy primVal)
substApp ty arg = do
  arg' <- evalTC arg
  param <- ask @"param"
  Eval.substValue param ty arg'

evalTC :: IR.Term primTy primVal
       -> InnerTC primTy primVal (IR.Value primTy primVal)
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
  π2 `Usage.allowsUsageOf` π1 && s2 <: s1 && t1 <: t2
s1 <: s2 = s1 == s2
infix 4 <: -- same as (<), etc
