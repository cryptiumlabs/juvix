module Juvix.Core.IR.Typechecker
  (module Juvix.Core.IR.Typechecker,
   -- reexports from ….Types
   T,
   Annotation (..),
   ContextElement (..), contextElement, Context, lookupCtx,
   TypecheckError (..), TypecheckerLog (..),
   EnvCtx (..), EnvAlias, EnvTypecheck (..), exec)
where

import qualified Juvix.Core.Parameterisation as Param
import qualified Juvix.Core.Usage as Usage
import Juvix.Core.IR.Typechecker.Types as Typed
import qualified Juvix.Core.IR.Evaluator as Eval
import qualified Juvix.Core.IR.Types as IR
import Juvix.Library hiding (show)
import Prelude (String, show)

logOutput ::
  forall m. HasWriter "typecheckerLog" [TypecheckerLog] m => String -> m ()
logOutput s = tell @"typecheckerLog" [TypecheckerLog s]

typeTermIntroLog ::
  forall primTy primVal m.
  ( HasThrow "typecheckError" (TypecheckError primTy primVal m) m,
    HasWriter "typecheckerLog" [TypecheckerLog] m,
    Show primTy,
    Show primVal,
    Show (IR.Value primTy primVal m),
    -- Dumb hack, please remove
    Show (Context primTy primVal m)
  ) =>
  IR.Term primTy primVal ->
  Annotation primTy primVal m ->
  Natural ->
  Context primTy primVal m ->
  m ()
typeTermIntroLog t ann ii g =
  logOutput
    ( ". The current context is "
        <> show g
        <> ". The current index is "
        <> show ii
        <> ". Type checking the term "
        <> show t
        <> "against the input annotation with usage of "
        <> (show (annUsage ann))
        <> ", and type of "
        <> (show (annType ann))
        <> ". "
        <> show t
    )

failed :: String
failed = "Check failed. "

passed :: String
passed = "Check passed. "

-- TODO ∷ Term isn't used here?
typechecked ::
  forall primTy primVal m.
  ( HasThrow "typecheckError" (TypecheckError primTy primVal m) m,
    HasWriter "typecheckerLog" [TypecheckerLog] m,
    Show primTy,
    Show primVal,
    (Show (IR.Value primTy primVal m))
  ) =>
  IR.Term primTy primVal ->
  Annotation primTy primVal m ->
  String
typechecked _term ann =
  "The term is type checked successfully. It has usage of "
    <> show (annUsage ann)
    <> "and type "
    <> show (annType ann)

-- | 'checker' for checkable terms checks the term
-- against an annotation and returns ().
typeTerm ::
  forall primTy primVal m.
  ( HasThrow "typecheckError" (TypecheckError primTy primVal m) m,
    HasWriter "typecheckerLog" [TypecheckerLog] m,
    Show primTy,
    Show primVal,
    Eq primTy,
    Eq primVal,
    (Show (IR.Value primTy primVal m)),
    -- Dumb hack
    (Show (Context primTy primVal m)),
    (Show (Annotation primTy primVal m))
  ) =>
  Param.Parameterisation primTy primVal ->
  Natural ->
  Context primTy primVal m ->
  IR.Term primTy primVal ->
  Annotation primTy primVal m ->
  m ()
-- ★ (Universe formation rule)
typeTerm _ ii g t@(IR.Star i) ann = do
  typeTermIntroLog t ann ii g
  logOutput
    ( concat
        [ "patterned matched to be a * term. Type checker ",
          "applies the universe formation rule. ",
          "Checking that Sigma is zero. "
        ]
    )
  unless
    (mempty == annUsage ann)
    $ do
      logOutput $
        failed
          <> "Sigma is "
          <> show (annUsage ann)
          <> ", which is not zero. "
      throw @"typecheckError" SigmaMustBeZero
  -- checks sigma = 0.
  logOutput $
    passed
      <> "The input usage "
      <> show (annUsage ann)
      <> "is zero. "
  logOutput "Checking that the annotation is of type *j, and j>i. "
  case annType ann of
    IR.VStar j ->
      if i >= j
        then
          ( do
              logOutput $
                failed
                  <> "The input annotation is of * level"
                  <> show j
                  <> ", which is not greater than the term's * level "
                  <> show i
                  <> ". "
              throw @"typecheckError" (UniverseMismatch t (annType ann))
          )
        else
          ( do
              logOutput $
                passed
                  <> "The input annotation is of * level"
                  <> show j
                  <> ", which is greater than the term's * level "
                  <> show i
                  <> ". "
                  <> typechecked t ann
              pure ()
          )
    _ -> do
      logOutput $
        failed
          <> " The input annotation "
          <> show ann
          <> " is not of * type, it is of type "
          <> show (annType ann)
      throw @"typecheckError" (ShouldBeStar (annType ann))
-- ★- Pi (Universe introduction rule)
typeTerm p ii g t@(IR.Pi _pi varType resultType) ann = do
  typeTermIntroLog t ann ii g
  logOutput
    ( concat
        [ "patterned matched to be a dependent function type term. ",
          "Type checker applies the universe introduction rule. ",
          "Checking that sigma is zero. "
        ]
    )
  unless -- checks sigma = 0.
    (Usage.SNat 0 == annUsage ann)
    ( do
        logOutput $
          failed
            <> "The input usage (sigma) is "
            <> show (annUsage ann)
            <> ", which is not zero. "
        throw @"typecheckError" SigmaMustBeZero
    )
  logOutput $
    passed
      <> "The input usage (sigma) is "
      <> show (annUsage ann)
      <> ", it is zero, as required. "
  logOutput $
    "Checking that the input type "
      <> show (annType ann)
      <> "is *i. "
  case annType ann of
    IR.VStar _ -> do
      logOutput $
        passed
          <> "The input type is "
          <> show (annType ann)
          <> ", it is *i, as required. "
      logOutput
        ( concat
            [ "Checking that the variable (V) type checked to ",
              "the input annotation, ",
              "which is just type checked to be of usage 0 and type *i.",
              "Checking that sigma is zero."
            ]
        )
      typeTerm p ii g varType ann
      logOutput $
        passed
          <> "The variable (V) is "
          <> show (annUsage ann)
          <> " and type "
          <> show (annType ann)
          <> ", as required. Checking that the result (R), with x of "
          <> "type V in the context, type checked against the input annotation."
      ty <- Eval.evalTerm p varType [] -- V
      typeTerm
        p -- param
        (succ ii)
        -- add x of type V, with zero usage to the context
        (contextElement (IR.Local ii) (Usage.SNat 0) ty : g)
        -- R, with x in the context
        (Eval.substTerm 0 (IR.Free (IR.Local ii)) resultType)
        -- is of 0 usage and type *i
        ann
      logOutput $
        " Current context is "
          <> show g
          <> passed
          <> "Result (R) has usage of is of type *i. "
          <> typechecked t ann
    _ -> do
      logOutput $
        " Current context is "
          <> show g
          <> failed
          <> "The annotation is not of type *, it is of type "
          <> show (annType ann)
      throw @"typecheckError" (ShouldBeStar (annType ann))
-- primitive types are of type *0 with 0 usage (typing rule missing from lang ref?)
typeTerm _ ii g x@(IR.PrimTy _) ann = do
  ty ← IR.quote0 (annType ann)
  typeTermIntroLog x ann ii g
  logOutput
    ( "patterned matched to be a primitive type term. "
        <> "Checking that input annotation is of zero usage. "
    )
  if  | mempty /= annUsage ann -> do
          logOutput $
            failed
              <> "The input usage is "
              <> show (annUsage ann)
              <> ", which is not zero. "
          throw @"typecheckError" (UsageMustBeZero)
      | ty /= IR.Star 0 -> do
          checking
          logOutput $
            failed
              <> "The input type is "
              <> show ty
              <> ", which is not * 0."
          throw @"typecheckError" $
            TypeMismatch ii x (Annotation (Usage.SNat 0) (IR.VStar 0)) ann
      | otherwise -> do
          checking
          logOutput $ passed <> typechecked x ann
          pure ()
  where
    checking = logOutput "Checking that input annotation is of type *0. "
-- Lam (introduction rule of dependent function type),
-- requires Pi (formation rule of dependent function type)
typeTerm p ii g t@(IR.Lam m) ann@(Annotation sig ty) = do
  typeTermIntroLog t ann ii g
  logOutput
    ( concat
        [ "patterned matched to be a Lam term. Checking that input annotation ",
          "has sigma usage and is of dependent function type (Pi). "
        ]
    )
  case ty of
    IR.VPi pi ty ty' -> do
      -- Lam m should be of dependent function type (Pi) with sigma usage.
      logOutput $
        passed
          <> "Checking that M, "
          <> show m
          <> " , with x, annotated with sig*pi ("
          <> show sig
          <> "*"
          <> show pi
          <> ") usage in the context, "
          <> "type checked against the input annotation."
      ty' <- ty' (IR.vfree (IR.Local ii))
        -- apply the function, result is of type T
      typeTerm
        p -- param
        (succ ii)
        -- put x in the context with usage sig*pi and type ty
        (contextElement (IR.Local ii) (sig <.> pi) ty : g)
        -- m, with x in the context
        (Eval.substTerm 0 (IR.Free (IR.Local ii)) m)
        -- is of type T with usage sigma
        (Annotation sig ty')
      logOutput $
        " The current context is "
          <> show g
          <> passed
          <> typechecked t ann
    _ -> throw @"typecheckError" (ShouldBeFunctionType (annType ann) (IR.Lam t))
--
typeTerm p ii g t@(IR.Elim e) ann = do
  typeTermIntroLog t ann ii g
  logOutput $
    "patterned matched to be an Elim term. Checking that input annotation "
      <> show ann
      <> " is compatible with the term "
      <> show t
  ann' <- typeElim p ii g e
  annt <- IR.quote0 (annType ann)
  annt' <- IR.quote0 (annType ann')
  if  | not (annUsage ann' `Usage.allowsUsageOf` annUsage ann) -> do
          logOutput $
            "Usages not compatible. The input usage is "
              <> show (annUsage ann)
              <> "but the term's usage is "
              <> show (annUsage ann')
          throw @"typecheckError" (UsageNotCompatible ann' ann)
      | annt /= annt' -> do
          logOutput $
            "The types are not the same. The input type is "
              <> show annt
              <> "but the term's type is "
              <> show annt'
          throw @"typecheckError" (TypeMismatch ii t ann ann')
      | otherwise -> pure ()

typeElimIntroLog ::
  forall primTy primVal m.
  ( HasThrow "typecheckError" (TypecheckError primTy primVal m) m,
    HasWriter "typecheckerLog" [TypecheckerLog] m,
    Show primTy,
    Show primVal,
    (Show (IR.Value primTy primVal m)),
    -- Hack, fix later
    (Show (Context primTy primVal m))
  ) =>
  IR.Elim primTy primVal ->
  Context primTy primVal m ->
  m ()
typeElimIntroLog elim g =
  logOutput
    ( "The current context is "
        <> show g
        <> ". Type checking the term "
        <> show elim
        <> ", which "
    )

-- inferable terms have type as output.
typeElim0 ::
  forall primTy primVal m.
  ( HasThrow "typecheckError" (TypecheckError primTy primVal m) m,
    HasWriter "typecheckerLog" [TypecheckerLog] m,
    Show primTy,
    Show primVal,
    Eq primTy,
    Eq primVal,
    -- Dumb hack
    (Show (Annotation primTy primVal m)),
    (Show (Context primTy primVal m)),
    (Show (IR.Value primTy primVal m))
  ) =>
  Param.Parameterisation primTy primVal ->
  Context primTy primVal m ->
  IR.Elim primTy primVal ->
  m (Annotation primTy primVal m)
typeElim0 p = typeElim p 0

typeElim ::
  forall primTy primVal m.
  ( HasThrow "typecheckError" (TypecheckError primTy primVal m) m,
    HasWriter "typecheckerLog" [TypecheckerLog] m,
    Show primTy,
    Show primVal,
    Eq primTy,
    Eq primVal,
    (Show (IR.Value primTy primVal m)),
    -- Dumb hack
    (Show (Context primTy primVal m)),
    (Show (Annotation primTy primVal m))
  ) =>
  Param.Parameterisation primTy primVal ->
  Natural ->
  Context primTy primVal m ->
  IR.Elim primTy primVal ->
  m (Annotation primTy primVal m)
-- the type checker should never encounter a
-- bound variable (as in LambdaPi)? To be confirmed.
typeElim _ _ii g e@(IR.Bound _) = do
  typeElimIntroLog e g
  logOutput
    "patterned matched to be a bound variable. Bound variables cannot be inferred."
  throw @"typecheckError" BoundVariableCannotBeInferred
typeElim _ ii g e@(IR.Free x) = do
  typeElimIntroLog e g
  logOutput $
    "patterned matched to be a free variable. "
      <> "Check that the free variable is in the context "
      <> show g
  case lookupCtx x g of
    Just ann -> do
      logOutput $
        passed
          <> "The variable is found in the context with annotation of usage "
          <> show (annUsage ann)
          <> "and type "
          <> show (annType ann)
      pure ann
    Nothing -> do
      logOutput $
        failed
          <> "cannot find "
          <> show e
          <> "in the context "
          <> show g
      throw @"typecheckError" (UnboundBinder ii x)
-- Prim-Const and Prim-Fn, pi = omega
typeElim p _ii g e@(IR.Prim prim) = do
  typeElimIntroLog e g
  logOutput "patterned matched to be a primitive type const/fn."
  let arrow (x :| []) = IR.VPrimTy x
      arrow (x :| (y : ys)) =
        IR.VPi Usage.Omega (IR.VPrimTy x) (const (pure (arrow (y :| ys))))
   in pure $ Annotation Usage.Omega (arrow (Param.typeOf p prim))
-- App, function M applies to N (Elimination rule of dependent function types)
typeElim p ii g e@(IR.App m n) = do
  typeElimIntroLog e g
  logOutput $
    concat
      [ "patterned matched to be an App term. Checking that M ",
        "has usage sigma and type dependent function (Pi)."
      ]
  Annotation sig mTy <- typeElim p ii g m
    -- annotation of M is usage sig and Pi with pi usage.
  case mTy of
    IR.VPi pi varTy resultTy → do
      logOutput $
        passed
          <> "The function (M) "
          <> show m
          <> " has usage "
          <> show sig
          <> " and dependent function type "
          <> show mTy
          <> " as required. Checking that the function argument (N)"
          <> show n
          <> " is of the argument type S ("
          <> show varTy
          <> ") with sigma*pi "
          <> show sig
          <> "*"
          <> show pi
          <> " usage. "
      -- N has to be of type S (varTy) with usage sig*pi
      typeTerm p ii g n $ Annotation (sig <.> pi) varTy
      logOutput $
        passed
          <> "The function argument (N) "
          <> show n
          <> " has usage "
          <> show (sig <.> pi)
          <> " and type "
          <> show varTy
      res <- resultTy =<< Eval.evalTerm p n [] -- T[x:=N]
      logOutput $
        show e
          <> "type checked to usage of "
          <> show sig
          <> " and type of "
          <> show res
      pure $ Annotation sig res
    _ -> do
      logOutput $
        failed
          <> "The function (M) "
          <> show m
          <> " is not of type dependent function."
      throw @"typecheckError" (MustBeFunction m ii n)
-- Conv
typeElim p ii g e@(IR.Ann pi theTerm theType) = do
  typeElimIntroLog e g
  logOutput
    ( concat
        [ "patterned matched to be an Ann term. Checking that theTerm ",
          show theTerm,
          "(M) has usage sigma and its type (S) is equivalent ",
          "to the input type (T), ",
          show theType
        ]
    )
  -- the input type, T
  ty <- Eval.evalTerm p theType []
  -- checks that M has usage sigma and type S == T
  typeTerm p ii g theTerm (Annotation pi ty)
  logOutput passed
  -- M has sigma usage and type T
  pure $ Annotation pi ty
