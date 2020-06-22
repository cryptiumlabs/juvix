module Juvix.Core.IR.Typechecker
  ( module Juvix.Core.IR.Typechecker,
    module Juvix.Core.IR.Typechecker.Log,
    -- reexports from ….Types
    T,
    Annotation' (..),
    Annotation,
    ContextElement' (..),
    ContextElement,
    contextElement,
    Context,
    lookupCtx,
    TypecheckError' (..),
    TypecheckError,
    getTermAnn,
    getElimAnn,
    -- reexports from ….Env
    EnvCtx (..),
    EnvAlias,
    EnvTypecheck (..),
    exec,
    Globals,
    Global (..),
  )
where

import qualified Data.HashMap.Strict as HashMap
import qualified Juvix.Core.IR.Evaluator as Eval
import Juvix.Core.IR.Typechecker.Env
import Juvix.Core.IR.Typechecker.Log
import Juvix.Core.IR.Typechecker.Types as Typed
import qualified Juvix.Core.IR.Types as IR
import qualified Juvix.Core.Parameterisation as Param
import qualified Juvix.Core.Usage as Usage
import Juvix.Library hiding (Datatype)

throwLog ::
  (HasLogTC primTy primVal m, HasThrowTC primTy primVal m) =>
  TypecheckError primTy primVal ->
  m z
throwLog err = do tellLog $ TcError err; throwTC err

isStar :: IR.Value primTy primVal -> Bool
isStar (IR.VStar _) = True
isStar _ = False

ensure ::
  (HasLogTC primTy primVal m, HasThrowTC primTy primVal m) =>
  Bool ->
  Log primTy primVal ->
  TypecheckError primTy primVal ->
  m ()
ensure cond msg err = if cond then tellLog msg else throwLog err

-- | 'checker' for checkable terms checks the term
-- against an annotation and returns ().
typeTerm ::
  forall primTy primVal m.
  ( HasThrowTC primTy primVal m,
    HasLogTC primTy primVal m,
    HasReader "globals" (Globals primTy primVal) m,
    Show primTy,
    Show primVal,
    Eq primTy,
    Eq primVal
  ) =>
  Param.Parameterisation primTy primVal ->
  Natural ->
  Context primTy primVal ->
  IR.Term primTy primVal ->
  Annotation primTy primVal ->
  m (Typed.Term primTy primVal)
-- ★ (Universe formation rule)
typeTerm _ _ii ctx tm@(IR.Star i) ann@(Annotation σ ty) = do
  tellLogs [TermIntro ctx tm ann, CheckingStar, CheckingSigmaZero]
  unless (σ == Usage.SNat 0) $ do
    -- checks sigma = 0.
    throwLog SigmaMustBeZero
  tellLogs [SigmaIsZero, CheckingLevels]
  case ty of
    IR.VStar j
      | i < j -> tellLog $ LevelOK i j
      | otherwise -> throwLog $ UniverseMismatch i j
    _ -> throwLog $ ShouldBeStar ty
  tellLog $ Typechecked tm ann
  pure $ Typed.Star i ann
-- ★- Pi (Universe introduction rule)
typeTerm p ii ctx tm@(IR.Pi pi varType resultType) ann@(Annotation σ ty) = do
  tellLogs [TermIntro ctx tm ann, CheckingPi, CheckingSigmaZero]
  ensure (σ == mempty) SigmaIsZero SigmaMustBeZero
  tellLog $ CheckingPiAnnIsStar ty
  ensure (isStar ty) (PiAnnIsStar ty) (ShouldBeStar ty)
  tellLog CheckingPiArg
  varType' <- typeTerm p ii ctx varType ann
  let varUniv = annType $ getTermAnn varType'
  tellLog CheckingPiRes
  ty <- Eval.evalTerm p varType
  resultType' <-
    typeTerm
      p
      (succ ii)
      -- add x of type V, with zero usage to the context
      (contextElement (IR.Local ii) (Usage.SNat 0) ty : ctx)
      -- R, with x in the context
      (Eval.substTerm (IR.Free (IR.Local ii)) resultType)
      -- is of 0 usage and type *i
      ann
  let resUniv = annType $ getTermAnn resultType'
  univ <- mergeStar varUniv resUniv
  let ann' = ann {annType = univ}
  tellLog $ Typechecked tm ann'
  pure $ Typed.Pi pi varType' resultType' ann'
  where
    mergeStar ::
      IR.Value primTy primVal ->
      IR.Value primTy primVal ->
      m (IR.Value primTy primVal)
    mergeStar (IR.VStar i) (IR.VStar j) = pure $ IR.VStar $ i `max` j
    mergeStar (IR.VStar _) ty = throwTC $ ShouldBeStar ty
    mergeStar ty _ = throwTC $ ShouldBeStar ty
-- ↑ we checked it was VStar on line 96
--   we just care about any unknown levels being filled in
-- primitive types are of type *0 with 0 usage (typing rule missing from lang ref?)
typeTerm _ ii ctx tm@(IR.PrimTy pty) ann@(Annotation σ ty) = do
  tellLogs [TermIntro ctx tm ann, CheckingPrimTy, CheckingSigmaZero]
  ensure (σ == mempty) SigmaIsZero UsageMustBeZero
  unless (isStar ty)
    $ throwLog
    $ TypeMismatch ii tm (IR.VStar 0) ty
  tellLog $ Typechecked tm ann
  pure $ Typed.PrimTy pty ann
-- Lam (introduction rule of dependent function type),
-- requires Pi (formation rule of dependent function type)
typeTerm p ii ctx tm@(IR.Lam m) ann@(Annotation σ ty) = do
  tellLogs [TermIntro ctx tm ann, CheckingLam]
  case ty of
    IR.VPi π ty1 ty2 -> do
      -- Lam m should be of dependent function type (Pi) with sigma usage.
      tellLogs [LamAnnIsPi, LamBodyWith σ π]
      -- apply the function, result is of type T
      ty2' <- Eval.substValue p (IR.VFree (IR.Local ii)) ty2
      let ctx' = contextElement (IR.Local ii) (σ <.> π) ty1 : ctx
          -- put x in the context with usage σπ and type ty1
          m' = Eval.substTerm (IR.Free (IR.Local ii)) m
          -- m, with x in the context
          ann' = Annotation σ ty2'
      -- is of type ty2 with usage σ
      mAnn <- typeTerm p (succ ii) ctx' m' ann'
      tellLog $ Typechecked tm ann
      pure $ Typed.Lam mAnn ann
    _ -> throwLog $ ShouldBeFunctionType ty tm
-- let case
typeTerm p ii ctx (IR.Let l b) ann = do
  tellLog CheckingLet
  l' <- typeElim p ii ctx l
  let ctx' = ContextElement (IR.Local ii) (getElimAnn l') : ctx
      b'   = Eval.substTerm (IR.Free (IR.Local ii)) b
  bAnn <- typeTerm p (succ ii) ctx' b' ann
  pure $ Typed.Let l' bAnn ann
-- elim case
typeTerm p ii ctx tm@(IR.Elim e) ann@(Annotation σ ty) = do
  tellLogs [TermIntro ctx tm ann, CheckingElim]
  e' <- typeElim p ii ctx e
  let Annotation σ' ty' = getElimAnn e'
  unless (σ' `Usage.allowsUsageOf` σ) $ do
    throwLog $ UsageNotCompatible σ' σ
  unless (ty' <: ty) $ do
    throwLog $ TypeMismatch ii tm ty ty'
  pure $ Typed.Elim e' ann

typeElim0 ::
  forall primTy primVal m.
  ( HasThrowTC primTy primVal m,
    HasLogTC primTy primVal m,
    HasReader "globals" (Globals primTy primVal) m,
    Show primTy,
    Show primVal,
    Eq primTy,
    Eq primVal
  ) =>
  Param.Parameterisation primTy primVal ->
  Context primTy primVal ->
  IR.Elim primTy primVal ->
  m (Typed.Elim primTy primVal)
typeElim0 p = typeElim p 0

typeElim ::
  forall primTy primVal m.
  ( HasThrowTC primTy primVal m,
    HasLogTC primTy primVal m,
    HasReader "globals" (Globals primTy primVal) m,
    Show primTy,
    Show primVal,
    Eq primTy,
    Eq primVal
  ) =>
  Param.Parameterisation primTy primVal ->
  Natural ->
  Context primTy primVal ->
  IR.Elim primTy primVal ->
  m (Typed.Elim primTy primVal)
-- the type checker should never encounter a
-- bound variable (as in LambdaPi)? To be confirmed.
typeElim _ _ii ctx elim@(IR.Bound _) = do
  tellLog $ ElimIntro ctx elim
  throwLog BoundVariableCannotBeInferred
typeElim _ ii ctx elim@(IR.Free x) = do
  globals <- ask @"globals"
  tellLogs [ElimIntro ctx elim, InferringFree]
  case lookupCtx x ctx <|> lookupGlobal x globals of
    Just ann -> do
      tellLog $ FoundFree ann
      pure $ Typed.Free x ann
    Nothing -> do
      throwLog $ UnboundBinder ii x
-- Prim-Const and Prim-Fn, pi = omega
typeElim p _ii ctx elim@(IR.Prim prim) = do
  tellLog $ ElimIntro ctx elim
  tellLog InferringPrim
  let ann = Annotation Usage.Omega (arrow (Param.typeOf p prim))
  pure $ Typed.Prim prim ann
  where
    arrow (x :| []) = IR.VPrimTy x
    arrow (x :| (y : ys)) =
      IR.VPi Usage.Omega (IR.VPrimTy x) (arrow $ y :| ys)
-- App, function M applies to N (Elimination rule of dependent function types)
typeElim p ii ctx elim@(IR.App m n) = do
  tellLog $ ElimIntro ctx elim
  tellLog InferringApp
  mAnn <- typeElim p ii ctx m
  let Annotation σ mTy = getElimAnn mAnn
  -- annotation of M is usage sig and Pi with pi usage.
  case mTy of
    IR.VPi π varTy resultTy -> do
      tellLog $ AppFunIsPi m σ mTy n
      nAnn <- typeTerm p ii ctx n $ Annotation (σ <.> π) varTy
      n' <- Eval.evalTerm p n
      res <- Eval.substValue p n' resultTy -- T[x:=N]
      tellLog $ AppInferredAs σ res
      pure $ Typed.App mAnn nAnn $ Annotation σ res
    _ -> do
      throwLog $ MustBeFunction m ii n
-- Conv
typeElim p ii ctx elim@(IR.Ann π theTerm theType level) = do
  tellLog $ ElimIntro ctx elim
  tellLog InferringAnn
  tellLog $ CheckingAnnIsType theType
  let tyAnn = Annotation (Usage.SNat 0) (IR.VStar level)
  theType' <- typeTerm p ii ctx theType tyAnn
  tellLog $ CheckingAnnTerm theTerm π theType
  ty <- Eval.evalTerm p theType -- the input type, T
  let ann = Annotation π ty
  theTerm' <- typeTerm p ii ctx theTerm ann
  pure $ Typed.Ann π theTerm' theType' level ann

lookupGlobal ::
  IR.Name ->
  Globals primTy primVal ->
  Maybe (Annotation primTy primVal)
lookupGlobal (IR.Local _) _ = Nothing
lookupGlobal (IR.Global x) globals =
  makeAnn <$> HashMap.lookup x globals
  where
    makeAnn (GDatatype (IR.Datatype {dataArgs, dataLevel})) =
      Annotation
        { annUsage = Usage.Omega,
          annType = foldr makePi (IR.VStar dataLevel) dataArgs
        }
      where
        makePi (IR.DataArg {argUsage, argType}) res = IR.VPi argUsage argType res
    makeAnn (GDataCon (IR.DataCon {conType})) =
      Annotation {annUsage = Usage.Omega, annType = conType}
    makeAnn (GFunction (IR.Function {funType})) =
      Annotation {annUsage = Usage.Omega, annType = funType}

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
infix 4 <: -- same as (<), etc

(<:) ::
  (Eq primTy, Eq primVal) =>
  IR.Value primTy primVal ->
  IR.Value primTy primVal ->
  Bool
IR.VStar i <: IR.VStar j = i <= j
IR.VPi π1 s1 t1 <: IR.VPi π2 s2 t2 =
  π2 `Usage.allowsUsageOf` π1 && s2 <: s1 && t1 <: t2
s1 <: s2 = s1 == s2

-- TODO: if PrimTys can ever be subtypes of each other the parameterisation will
-- need to know about that
