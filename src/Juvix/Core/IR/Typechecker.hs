module Juvix.Core.IR.Typechecker
  (module Juvix.Core.IR.Typechecker,
   module Juvix.Core.IR.Typechecker.Log,
   -- reexports from ….Types
   T,
   Annotation' (..), Annotation,
   ContextElement' (..), ContextElement, contextElement, Context, lookupCtx,
   TypecheckError' (..), TypecheckError, Log (..),
   EnvCtx (..), EnvAlias, EnvTypecheck (..), exec,
   getTermAnn, getElimAnn)
where

import qualified Juvix.Core.Parameterisation as Param
import qualified Juvix.Core.Usage as Usage
import Juvix.Core.IR.Typechecker.Log
import Juvix.Core.IR.Typechecker.Types as Typed
import qualified Juvix.Core.IR.Evaluator as Eval
import qualified Juvix.Core.IR.Types as IR
import Juvix.Library


type HasThrowTc' ext primTy primVal m =
  HasThrow "typecheckError" (TypecheckError' ext primTy primVal) m

type HasThrowTc primTy primVal m = HasThrowTc' IR.NoExt primTy primVal m

throwTc :: HasThrowTc primTy primVal m
        => TypecheckError primTy primVal -> m z
throwTc = throw @"typecheckError"

throwLog :: (HasLogTc primTy primVal m, HasThrowTc primTy primVal m)
         => TypecheckError primTy primVal -> m z
throwLog err = do tellLog $ TcError err; throwTc err


-- | 'checker' for checkable terms checks the term
-- against an annotation and returns ().
typeTerm ::
  ∀ primTy primVal m.
  ( HasThrowTc primTy primVal m,
    HasLogTc primTy primVal m,
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
typeTerm _ ii g t@(IR.Star i) ann@(Annotation σ ty) = do
  tellLog $ TermIntro ctx tm ann
  tellLog CheckingStar
  tellLog CheckingSigmaZero
  unless (σ == Usage.SNat 0) $ do -- checks sigma = 0.
    throwLog SigmaMustBeZero
  tellLog SigmaIsZero
  tellLog CheckingLevels
  case annType ann of
    IR.VStar j
      | i >= j -> do
          throwLog $ UniverseMismatch t ty
      | otherwise -> do
          tellLog $ LevelOK i j
          tellLog $ Typechecked t ann
          pure $ Typed.Star i ann
    _ -> do
      throwLog $ ShouldBeStar ty
-- ★- Pi (Universe introduction rule)
typeTerm p ii g t@(IR.Pi pi varType resultType) ann@(Annotation σ ty) = do
  tellLog $ TermIntro ctx tm ann
  tellLog CheckingPi
  tellLog CheckingSigmaZero
  unless (σ == Usage.SNat 0) $ do -- checks sigma = 0.
    throwLog SigmaMustBeZero
  tellLog SigmaIsZero
  tellLog $ CheckingPiAnnIsStar ty
  case annType ann of
    IR.VStar _ -> do
      tellLog $ PiAnnIsStar ty
      tellLog CheckingPiArg
      varType' <- typeTerm p ii g varType ann
      tellLog CheckingPiRes
      ty <- Eval.evalTerm p varType
      resultType' <- typeTerm p (succ ii)
        -- add x of type V, with zero usage to the context
        (contextElement (IR.Local ii) (Usage.SNat 0) ty : g)
        -- R, with x in the context
        (Eval.substTerm (IR.Free (IR.Local ii)) resultType)
        -- is of 0 usage and type *i
        ann
      tellLog $ Typechecked t ann
      pure $ Typed.Pi pi varType' resultType' ann
    _ -> do
      throwLog $ ShouldBeStar ty
-- primitive types are of type *0 with 0 usage (typing rule missing from lang ref?)
typeTerm _ ii g x@(IR.PrimTy pty) ann@(Annotation σ ty) = do
  tellLog $ TermIntro ctx tm ann
  tellLog CheckingPrimTy
  tellLog CheckingSigmaZero
  unless (σ == Usage.SNat 0) $ do
    throwLog UsageMustBeZero
  tellLog SigmaIsZero
  unless (isStar ty) $ do
    throwLog $ TypeMismatch ii x (Annotation (Usage.SNat 0) (IR.VStar 0)) ann
  tellLog $ Typechecked x ann
  pure $ Typed.PrimTy pty ann
 where
  isStar (IR.VStar _) = True
  isStar _            = False
-- Lam (introduction rule of dependent function type),
-- requires Pi (formation rule of dependent function type)
typeTerm p ii g t@(IR.Lam m) ann@(Annotation σ ty) = do
  tellLog $ TermIntro ctx tm ann
  tellLog CheckingLam
  case ty of
    IR.VPi π ty1 ty2 -> do
      -- Lam m should be of dependent function type (Pi) with sigma usage.
      tellLog LamAnnIsPi
      tellLog $ LamBodyWith σ π
      -- apply the function, result is of type T
      ty2' <- Eval.substValue p (IR.VFree (IR.Local ii)) ty2
      let g'   = contextElement (IR.Local ii) (σ <.> π) ty1 : g
            -- put x in the context with usage σπ and type ty1
          m'   = Eval.substTerm (IR.Free (IR.Local ii)) m
            -- m, with x in the context
          ann' = Annotation σ ty2'
            -- is of type ty2 with usage σ
      mAnn <- typeTerm p (succ ii) g' m' ann'
      tellLog $ Typechecked t ann
      pure $ Typed.Lam mAnn ann
    _ -> throwLog $ ShouldBeFunctionType ty t
-- elim case
typeTerm p ii g t@(IR.Elim e) ann@(Annotation σ ty) = do
  tellLog $ TermIntro ctx tm ann
  tellLog CheckingElim
  e' <- typeElim p ii g e
  let ann'@(Annotation σ' ty') = getElimAnn e'
  unless (σ' `Usage.allowsUsageOf` σ) $ do
    throwLog $ UsageNotCompatible ann' ann
  unless (ty == ty') $ do -- TODO subtyping
    throwLog $ TypeMismatch ii t ann ann'
  pure $ Typed.Elim e' ann


typeElim0 ::
  ∀ primTy primVal m.
  ( HasThrowTc primTy primVal m,
    HasLogTc primTy primVal m,
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
  ( HasThrowTc primTy primVal m,
    HasLogTc primTy primVal m,
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
typeElim _ _ii g e@(IR.Bound _) = do
  tellLog $ ElimIntro g elim
  throwLog BoundVariableCannotBeInferred
typeElim _ ii g e@(IR.Free x) = do
  tellLog $ ElimIntro g elim
  tellLog InferringFree
  case lookupCtx x g of
    Just ann -> do
      tellLog $ FoundFree ann
      pure $ Typed.Free x ann
    Nothing -> do
      throwLog $ UnboundBinder ii x
-- Prim-Const and Prim-Fn, pi = omega
typeElim p _ii g e@(IR.Prim prim) = do
  tellLog $ ElimIntro g elim
  tellLog InferringPrim
  let ann = Annotation Usage.Omega (arrow (Param.typeOf p prim))
  pure $ Typed.Prim prim ann
 where
  arrow (x :| []) = IR.VPrimTy x
  arrow (x :| (y : ys)) =
    IR.VPi Usage.Omega (IR.VPrimTy x) (arrow $ y :| ys)
-- App, function M applies to N (Elimination rule of dependent function types)
typeElim p ii g e@(IR.App m n) = do
  tellLog $ ElimIntro g elim
  tellLog InferringApp
  mAnn <- typeElim p ii g m
  let Annotation σ mTy = getElimAnn mAnn
    -- annotation of M is usage sig and Pi with pi usage.
  case mTy of
    IR.VPi π varTy resultTy -> do
      tellLog $ AppFunIsPi m σ mTy n
      nAnn <- typeTerm p ii g n $ Annotation (σ <.> π) varTy
      n' <- Eval.evalTerm p n
      res <- Eval.substValue p n' resultTy -- T[x:=N]
      tellLog $ AppInferredAs σ res
      pure $ Typed.App mAnn nAnn $ Annotation σ res
    _ -> do
      throwLog $ MustBeFunction m ii n
-- Conv
typeElim p ii g e@(IR.Ann π theTerm theType) = do
  tellLog $ ElimIntro g elim
  tellLog InferringAnn
  tellLog $ CheckingAnnIsType theType
  theType' <- typeTerm p ii g theType $ Annotation mempty (IR.VStar _univ)
  tellLog $ CheckingAnnTerm theTerm π theType
  ty <- Eval.evalTerm p theType -- the input type, T
  let ann = Annotation π ty -- M has sigma usage and type T
  theTerm' <- typeTerm p ii g theTerm ann
  pure $ Typed.Ann π theTerm' theType' ann
