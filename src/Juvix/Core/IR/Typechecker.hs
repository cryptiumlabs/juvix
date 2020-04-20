module Juvix.Core.IR.Typechecker
  (module Juvix.Core.IR.Typechecker,
   module Juvix.Core.IR.Typechecker.Log,
   -- reexports from ….Types
   T,
   Annotation' (..), Annotation,
   ContextElement' (..), ContextElement, contextElement, Context, lookupCtx,
   TypecheckError' (..), TypecheckError,
   getTermAnn, getElimAnn,
   -- reexports from ….Env
   EnvCtx (..), EnvAlias, EnvTypecheck (..), exec)
where

import qualified Juvix.Core.Parameterisation as Param
import qualified Juvix.Core.Usage as Usage
import Juvix.Core.IR.Typechecker.Log
import Juvix.Core.IR.Typechecker.Env
import Juvix.Core.IR.Typechecker.Types as Typed
import qualified Juvix.Core.IR.Evaluator as Eval
import qualified Juvix.Core.IR.Types as IR
import Juvix.Library


throwLog :: (HasLogTC primTy primVal m, HasThrowTC primTy primVal m)
         => TypecheckError primTy primVal -> m z
throwLog err = do tellLog $ TcError err; throwTC err

isStar :: IR.Value primTy primVal -> Bool
isStar (IR.VStar _) = True
isStar _            = False

-- | 'checker' for checkable terms checks the term
-- against an annotation and returns ().
typeTerm ::
  ∀ primTy primVal m.
  ( HasThrowTC primTy primVal m,
    HasLogTC primTy primVal m,
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
  unless (σ == Usage.SNat 0) $ do -- checks sigma = 0.
    throwLog SigmaMustBeZero
  tellLogs [SigmaIsZero, CheckingLevels]
  case ty of
    IR.VStar j | i < j     -> tellLog  $ LevelOK i j
               | otherwise -> throwLog $ UniverseMismatch i j
    _                      -> throwLog $ ShouldBeStar ty
  tellLog $ Typechecked tm ann
  pure $ Typed.Star i ann
-- ★- Pi (Universe introduction rule)
typeTerm p ii ctx tm@(IR.Pi pi varType resultType) ann@(Annotation σ ty) = do
  tellLogs [TermIntro ctx tm ann, CheckingPi, CheckingSigmaZero]
  if σ == Usage.SNat 0 then -- checks sigma = 0.
    tellLog SigmaIsZero
  else
    throwLog SigmaMustBeZero
  tellLog $ CheckingPiAnnIsStar ty
  if isStar ty then
    tellLog $ PiAnnIsStar ty
  else
    throwLog $ ShouldBeStar ty
  tellLog CheckingPiArg
  varType' <- typeTerm p ii ctx varType ann
  let varUniv = annType $ getTermAnn varType'
  tellLog CheckingPiRes
  ty <- Eval.evalTerm p varType
  resultType' <- typeTerm p (succ ii)
    -- add x of type V, with zero usage to the context
    (contextElement (IR.Local ii) (Usage.SNat 0) ty : ctx)
    -- R, with x in the context
    (Eval.substTerm (IR.Free (IR.Local ii)) resultType)
    -- is of 0 usage and type *i
    ann
  let resUniv = annType $ getTermAnn resultType'
  univ <- mergeStar varUniv resUniv
  let ann'    = ann {annType = univ}
  tellLog $ Typechecked tm ann'
  pure $ Typed.Pi pi varType' resultType' ann'
 where
  mergeStar :: IR.Value primTy primVal -> IR.Value primTy primVal
            -> m (IR.Value primTy primVal)
  mergeStar (IR.VStar i) (IR.VStar j) = pure $ IR.VStar $ i `max` j
  mergeStar (IR.VStar _) ty           = throwTC $ ShouldBeStar ty
  mergeStar ty           _            = throwTC $ ShouldBeStar ty
    -- ↑ we checked it was VStar on line 96
    --   we just care about any unknown levels being filled in
-- primitive types are of type *0 with 0 usage (typing rule missing from lang ref?)
typeTerm _ ii ctx tm@(IR.PrimTy pty) ann@(Annotation σ ty) = do
  tellLogs [TermIntro ctx tm ann, CheckingPrimTy, CheckingSigmaZero]
  if σ == Usage.SNat 0 then
    tellLog SigmaIsZero
  else
    throwLog UsageMustBeZero
  unless (isStar ty) $
    throwLog $ TypeMismatch ii tm (IR.VStar 0) ty
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
          m'   = Eval.substTerm (IR.Free (IR.Local ii)) m
            -- m, with x in the context
          ann' = Annotation σ ty2'
            -- is of type ty2 with usage σ
      mAnn <- typeTerm p (succ ii) ctx' m' ann'
      tellLog $ Typechecked tm ann
      pure $ Typed.Lam mAnn ann
    _ -> throwLog $ ShouldBeFunctionType ty tm
-- elim case
typeTerm p ii ctx tm@(IR.Elim e) ann@(Annotation σ ty) = do
  tellLogs [TermIntro ctx tm ann, CheckingElim]
  e' <- typeElim p ii ctx e
  let Annotation σ' ty' = getElimAnn e'
  unless (σ' `Usage.allowsUsageOf` σ) $ do
    throwLog $ UsageNotCompatible σ' σ
  unless (ty == ty') $ do -- TODO subtyping
    throwLog $ TypeMismatch ii tm ty ty'
  pure $ Typed.Elim e' ann


typeElim0 ::
  ∀ primTy primVal m.
  ( HasThrowTC primTy primVal m,
    HasLogTC primTy primVal m,
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
  ∀ primTy primVal m.
  ( HasThrowTC primTy primVal m,
    HasLogTC primTy primVal m,
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
  tellLogs [ElimIntro ctx elim, InferringFree]
  case lookupCtx x ctx of
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
