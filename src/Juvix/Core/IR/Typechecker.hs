module Juvix.Core.IR.Typechecker where

import Juvix.Core.IR.Evaluator
import Juvix.Core.IR.Types
import Juvix.Core.Types
import Juvix.Core.Usage
import Juvix.Library hiding (show)
import Prelude (String, lookup, show)

logOutput ∷ ∀ m. HasWriter "typecheckerLog" [TypecheckerLog] m ⇒ String → m ()
logOutput s = tell @"typecheckerLog" [TypecheckerLog s]

typeTermIntroLog ∷
  ∀ primTy primVal m.
  ( HasThrow "typecheckError" (TypecheckError primTy primVal m) m,
    HasWriter "typecheckerLog" [TypecheckerLog] m,
    Show primTy,
    Show primVal,
    (Show (Value primTy primVal m))
  ) ⇒
  Term primTy primVal →
  Annotation primTy primVal m →
  m ()
typeTermIntroLog t ann =
  logOutput
    ( "Type checking the term "
        <> (show t)
        <> "against the input annotation with usage of "
        <> (show (fst ann))
        <> ", and type of "
        <> (show (snd ann))
        <> ". "
        <> (show t)
    )

failed ∷ String
failed = "Check failed. "

passed ∷ String
passed = "Check passed. "

typechecked ∷
  ∀ primTy primVal m.
  ( HasThrow "typecheckError" (TypecheckError primTy primVal m) m,
    HasWriter "typecheckerLog" [TypecheckerLog] m,
    Show primTy,
    Show primVal,
    (Show (Value primTy primVal m))
  ) ⇒
  Term primTy primVal →
  Annotation primTy primVal m →
  String
typechecked term ann =
  "The term is type checked successfully. It has usage of "
    <> show (fst ann)
    <> "and type "
    <> show (snd ann)

-- | 'checker' for checkable terms checks the term against an annotation and returns ().
typeTerm ∷
  ∀ primTy primVal m.
  ( HasThrow "typecheckError" (TypecheckError primTy primVal m) m,
    HasWriter "typecheckerLog" [TypecheckerLog] m,
    Show primTy,
    Show primVal,
    Eq primTy,
    Eq primVal,
    (Show (Value primTy primVal m))
  ) ⇒
  Parameterisation primTy primVal →
  Natural →
  Context primTy primVal m →
  Term primTy primVal →
  Annotation primTy primVal m →
  m ()

-- * (Universe formation rule)

typeTerm _ _ii _g t@(Star i) ann = do
  typeTermIntroLog t ann
  logOutput "patterned matched to be a * term. Type checker applies the universe formation rule. Checking that Sigma is zero. "
  unless
    (SNat 0 == fst ann)
    ( do
        logOutput $
          failed
            <> "Sigma is "
            <> show (fst ann)
            <> ", which is not zero. "
        throw @"typecheckError" SigmaMustBeZero
    ) -- checks sigma = 0.
  logOutput $
    passed
      <> "The input usage "
      <> show (fst ann)
      <> "is zero. "
  logOutput "Checking that the annotation is of type *j, and j>i. "
  case (snd ann) of
    VStar j →
      if (i >= j)
        then
          ( do
              logOutput $
                failed
                  <> "The input annotation is of * level"
                  <> show j
                  <> ", which is not greater than the term's * level "
                  <> show i
                  <> ". "
              throw @"typecheckError" (UniverseMismatch t (snd ann))
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
              return ()
          )
    _ → do
      logOutput $
        failed
          <> " The input annotation is not of * type, it is of type "
          <> show (snd ann)
      throw @"typecheckError" (ShouldBeStar (snd ann))
  -- *-Pi (Universe introduction rule)
typeTerm p ii g t@(Pi _pi varType resultType) ann = do
  typeTermIntroLog t ann
  logOutput "patterned matched to be a dependent function type term. Type checker applies the universe introduction rule. Checking that sigma is zero."
  unless -- checks sigma = 0.
    (SNat 0 == fst ann)
    ( do
        logOutput $
          failed
            <> "The input usage (sigma) is "
            <> show (fst ann)
            <> ", which is not zero. "
        throw @"typecheckError" SigmaMustBeZero
    )
  logOutput $
    passed
      <> "The input usage (sigma) is "
      <> show (fst ann)
      <> ", it is zero, as required. "
  logOutput $
    "Checking that the input type "
      <> show (snd ann)
      <> "is *i. "
  case snd ann of
    VStar _ → do
      logOutput $
        passed
          <> "The input type is "
          <> show (snd ann)
          <> ", it is *i, as required. "
      logOutput $
        "Checking that the variable (V) type checked to the input annotation, which is just type checked to be of usage 0 and type *i. "
      typeTerm p ii g varType ann
      logOutput $
        passed
          <> "The variable (V) is "
          <> show (fst ann)
          <> " and type "
          <> show (snd ann)
          <> ", as required. Checking that the result (R), with x of type V in the context, type checked against the input annotation. "
      ty ← evalTerm p varType [] -- V
      typeTerm
        p -- param
        (ii + 1)
        ((Local ii, (SNat 0, ty)) : g) -- add x of type V, with zero usage to the context
        (substTerm 0 (Free (Local ii)) resultType) -- R, with x in the context 
        ann -- is of 0 usage and type *i
      logOutput $
        passed
          <> "Result (R) has usage of is of type *i. "
          <> typechecked t ann
    _ → do
      logOutput $
        failed
          <> "The annotation is not of type *, it is of type "
          <> show (snd ann)
      throw @"typecheckError" (ShouldBeStar (snd ann))
-- primitive types are of type *0 with 0 usage (typing rule missing from lang ref?)
typeTerm _ ii _g x@(PrimTy _) ann = do
  ty ← quote0 (snd ann)
  typeTermIntroLog x ann
  logOutput
    ("patterned matched to be a primitive type term. Checking that input annotation is of zero usage. ")
  if (SNat 0 /= fst ann)
    then
      ( do
          logOutput $
            failed
              <> "The input usage is "
              <> show (fst ann)
              <> ", which is not zero. "
          throw @"typecheckError" (UsageMustBeZero)
      )
    else do
      logOutput "Checking that input annotation is of type *0. "
      if (ty /= Star 0)
        then
          ( do
              logOutput $
                failed
                  <> "The input type is "
                  <> show ty
                  <> ", which is not * 0. "
              throw @"typecheckError" (TypeMismatch ii x (SNat 0, VStar 0) ann)
          )
        else do
          logOutput $ passed <> typechecked x ann
          return ()
-- Lam (introduction rule of dependent function type), requires Pi (formation rule of dependent function type)
typeTerm p ii g t@(Lam m) ann = do
  typeTermIntroLog t ann
  logOutput
    "patterned matched to be a Lam term. Checking that input annotation has sigma usage and is dependent function type (Pi). "
  case ann of
    (sig, VPi pi ty ty') → do
      -- Lam m should be of dependent function type (Pi) with sigma usage. 
      logOutput $ 
        "Checking that input usage " <> 
        show sig <>
        " is 0 or 1. "
      unless  
        (sig == SNat 0 || sig == SNat 1)
        (do
          logOutput $
            failed <> "Sigma is not 0 or 1. "
          throw @"typecheckError" (SigmaMustBe0or1)
        )
      logOutput $ 
        passed <> 
        "Checking that M, with x (annotated with sig*pi usage) in the context, type checked against the input annotation. "
      ty' ← ty' (vfree (Local ii)) -- apply the function, result is of type T
      typeTerm
        p -- param
        (ii + 1)
        ((Local ii, (sig <.> pi, ty)) : g) -- put x in the context with usage sig*pi and type ty
        (substTerm 0 (Free (Local ii)) m) -- m, with x in the context
        (sig, ty') -- is of type T with usage sigma
      logOutput $ 
        passed <> typechecked t ann
    _ → throw @"typecheckError" (ShouldBeFunctionType (snd ann) (Lam m))
-- 
typeTerm p ii g t@(Elim e) ann = do
  typeTermIntroLog t ann
  ann' ← typeElim p ii g e
  annt ← quote0 (snd ann)
  annt' ← quote0 (snd ann')
  if not (fst ann' `allowsUsageOf` fst ann)
    then
      ( do
          logOutput "Usages not compatible. "
          throw @"typecheckError" (UsageNotCompatible ann' ann)
      )
    else
      if (annt /= annt')
        then
          ( do
              logOutput "The types are not the same. "
              throw @"typecheckError" (TypeMismatch ii (Elim e) ann ann')
          )
        else return ()

typeElimIntroLog ∷
  ∀ primTy primVal m.
  ( HasThrow "typecheckError" (TypecheckError primTy primVal m) m,
    HasWriter "typecheckerLog" [TypecheckerLog] m,
    Show primTy,
    Show primVal,
    (Show (Value primTy primVal m))
  ) ⇒
  Elim primTy primVal →
  m ()
typeElimIntroLog elim =
  logOutput
    ( "Type checking the term "
        <> (show elim)
        <> ", which "
    )

-- inferable terms have type as output.
typeElim0 ∷
  ∀ primTy primVal m.
  ( HasThrow "typecheckError" (TypecheckError primTy primVal m) m,
    HasWriter "typecheckerLog" [TypecheckerLog] m,
    Show primTy,
    Show primVal,
    Eq primTy,
    Eq primVal,
    (Show (Value primTy primVal m))
  ) ⇒
  Parameterisation primTy primVal →
  Context primTy primVal m →
  Elim primTy primVal →
  m (Annotation primTy primVal m)
typeElim0 p = typeElim p 0

typeElim ∷
  ∀ primTy primVal m.
  ( HasThrow "typecheckError" (TypecheckError primTy primVal m) m,
    HasWriter "typecheckerLog" [TypecheckerLog] m,
    Show primTy,
    Show primVal,
    Eq primTy,
    Eq primVal,
    (Show (Value primTy primVal m))
  ) ⇒
  Parameterisation primTy primVal →
  Natural →
  Context primTy primVal m →
  Elim primTy primVal →
  m (Annotation primTy primVal m)
-- the type checker should never encounter a bound variable (as in LambdaPi)? To be confirmed.
typeElim _ _ii _g e@(Bound _) = do
  typeElimIntroLog e
  logOutput
    ("patterned matched to be a bound variable. Bound variables cannot be inferred. ")
  throw @"typecheckError" BoundVariableCannotBeInferred
typeElim _ ii g e@(Free x) = do
  typeElimIntroLog e
  logOutput $
    "patterned matched to be a free variable. Looking up the free variable in the context " 
    <> show g
  case lookup x g of
    Just ann → do
      logOutput $
        passed
          <> "The variable is found in the context with annotation of usage "
          <> show (fst ann)
          <> "and type "
          <> show (snd ann)
      return ann
    Nothing → do
      logOutput $
        failed
          <> "cannot find "
          <> show e
          <> "in the context "
          <> show g
      throw @"typecheckError" (UnboundBinder ii x)
-- Prim-Const and Prim-Fn, pi = omega
typeElim p _ii _g e@(Prim prim) = do
  typeElimIntroLog e
  logOutput "patterned matched to be a primitive type const/fn. "
  let arrow (x :| []) = VPrimTy x
      arrow (x :| (y : ys)) = VPi Omega (VPrimTy x) (const (pure (arrow (y :| ys))))
   in do
    pure (Omega, (arrow (Juvix.Core.Types.typeOf p prim)))
    --logOutput $ show e <> " type checked to usage " <> show Omega <> " and type " <> show ty
-- App, function M applies to N (Elimination rule of dependent function types)
typeElim p ii g e@(App m n) = do
  typeElimIntroLog e
  logOutput "patterned matched to be an App term. Checking that M has usage sigma and type dependent function (Pi). " 
  mTy ← typeElim p ii g m -- annotation of M is usage sig and Pi with pi usage.
  case mTy of
    (sig, VPi pi varTy resultTy) → do
      logOutput $ 
        "Checking that input usage " <> 
        show sig <>
        " is 0 or 1. "
      unless  
        (sig == SNat 0 || sig == SNat 1)
        (do
          logOutput $
            failed <> "Sigma is not 0 or 1. "
          throw @"typecheckError" (SigmaMustBe0or1)
        )
      logOutput $
        passed <>
        "The function (M) has usage " <>
        show sig <>
        " and dependent function type " <>
        show (snd mTy) <>
        " as required. Checking that the function argument (N) is of the argument type (S) with sigma*pi usage. "
      typeTerm p ii g n (sig <.> pi, varTy) -- N has to be of type S (varTy) with usage sig*pi
      logOutput $
        passed <>
        "The function argument (N) has usage " <>
        show (sig <.> pi) <>
        " and type " <>
        show varTy
      res ← resultTy =<< evalTerm p n [] -- T[x:=N]
      logOutput $ 
        show e <>
        "type checked to usage of " <>
        show sig <>
        " and type of " <>
        show res
      return (sig, res)
    _ → do
      logOutput $
        failed <> 
        "The function (M) is not of type dependent function. "  
      throw @"typecheckError" (MustBeFunction m ii n)
-- Conv
typeElim p ii g e@(Ann pi theTerm theType) = do
  typeElimIntroLog e
  logOutput "patterned matched to be an Ann term. Checking that theTerm (M) has usage sigma and its type (S) is equivalent to the input type (T). " 
  ty ← evalTerm p theType [] -- the input type, T
  typeTerm p ii g theTerm (pi, ty) -- checks that M has usage sigma and type S == T
  logOutput $ passed 
  return (pi, ty) -- M has sigma usage and type T
