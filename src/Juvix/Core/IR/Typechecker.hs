module Juvix.Core.IR.Typechecker where

import Juvix.Core.IR.Evaluator
import Juvix.Core.IR.Types
import Juvix.Core.Types
import Juvix.Core.Usage
import Juvix.Library hiding (show)
import Prelude (String, lookup, show)

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

data TypecheckerLog = TypecheckerLog {msg ∷ String}
  deriving (Eq, Show)

logOutput ∷ ∀ m. HasWriter "typecheckerLog" [TypecheckerLog] m ⇒ String → m ()
logOutput s = tell @"typecheckerLog" [TypecheckerLog s]

checkerIntroLog ∷
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
checkerIntroLog t ann =
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

-- * (Universe formation rule)

typeTerm _ _ii _g t@(Star i) ann = do
  checkerIntroLog t ann
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
              return ()
          )
    _ → do
      logOutput $
        failed
          <> " The annotation is not of type *, it is of type "
          <> show (snd ann)
      throw @"typecheckError" (ShouldBeStar (snd ann))
typeTerm p ii g t@(Pi pi varType resultType) ann = do
  checkerIntroLog t ann
  logOutput "patterned matched to be a dependent function type term. Type checker applies the universe introduction rule. Checking that sigma is zero."
  unless -- checks sigma = 0.
    (SNat 0 == fst ann)
    ( do
        logOutput $
          failed
            <> "Sigma is "
            <> show (fst ann)
            <> ", which is not zero. "
        throw @"typecheckError" SigmaMustBeZero
    )
  case snd ann of -- checks that type is *i
    VStar _ → do
      logOutput "Checking that the variable is of type *i. "
      typeTerm p ii g varType ann -- checks varType is of type Star i
      logOutput $
        passed
          <> "Variable is of type "
          <> show (snd ann)
          <> ". Checking that the result is of type *i. "
      ty ← evalTerm p varType []
      typeTerm
        p -- checks resultType is of type Star i
        (ii + 1)
        ((Local ii, (pi, ty)) : g)
        (substTerm 0 (Free (Local ii)) resultType)
        ann
    -- TODO: logOutput "Result is of type *i. The variable and the result are at the same * level."
    _ → do
      logOutput "The annotation is not of type *. "
      throw @"typecheckError" (ShouldBeStar (snd ann))
-- primitive types are of type *0 with 0 usage (typing rule missing from lang ref?)
typeTerm _ ii _g x@(PrimTy _) ann = do
  ty ← quote0 (snd ann)
  checkerIntroLog x ann
  logOutput
    ("patterned matched to be a primitive type term. Checking that input annotation is of zero usage and type * 0")
  if (SNat 0 /= fst ann)
    then
      ( do
          logOutput "Usage is not zero. "
          throw @"typecheckError" (UsageMustBeZero)
      )
    else
      if (ty /= Star 0)
        then
          ( do
              logOutput "Type is not * 0"
              throw @"typecheckError" (TypeMismatch ii x (SNat 0, VStar 0) ann)
          )
        else return ()
-- Lam (introduction rule of dependent function type)
typeTerm p ii g (Lam s) ann =
  case ann of
    (sig, VPi pi ty ty') → do
      -- Lam s should be of dependent function type (Pi pi ty ty').
      ty' ← ty' (vfree (Local ii))
      typeTerm
        p
        (ii + 1)
        ((Local ii, (sig <.> pi, ty)) : g) -- put s in the context with usage sig*pi
        (substTerm 0 (Free (Local ii)) s) -- x (varType) in context S with sigma*pi usage.
        (sig, ty') -- is of type M (usage sigma) in context T
    _ → throw @"typecheckError" (ShouldBeFunctionType (snd ann) (Lam s))
--
typeTerm p ii g (Elim e) ann = do
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
              logOutput "Annotation types are not the same. "
              throw @"typecheckError" (TypeMismatch ii (Elim e) ann ann')
          )
        else (throw @"typecheckError" (TypeMismatch ii (Elim e) ann ann'))

-- inferable terms have type as output.
typeElim0 ∷
  ∀ primTy primVal m.
  ( HasThrow "typecheckError" (TypecheckError primTy primVal m) m,
    Show primTy,
    Show primVal,
    Eq primTy,
    Eq primVal
  ) ⇒
  Parameterisation primTy primVal →
  Context primTy primVal m →
  Elim primTy primVal →
  m (Annotation primTy primVal m)
typeElim0 p = typeElim p 0

typeElim ∷
  ∀ primTy primVal m.
  ( HasThrow "typecheckError" (TypecheckError primTy primVal m) m,
    Show primTy,
    Show primVal,
    Eq primTy,
    Eq primVal
  ) ⇒
  Parameterisation primTy primVal →
  Natural →
  Context primTy primVal m →
  Elim primTy primVal →
  m (Annotation primTy primVal m)
-- the type checker should never encounter a bound variable (as in LambdaPi)? To be confirmed.
typeElim _ _ii _g (Bound _) = throw @"typecheckError" BoundVariableCannotBeInferred
typeElim _ ii g (Free x) =
  case lookup x g of
    Just ann → return ann
    Nothing → throw @"typecheckError" (UnboundBinder ii x)
-- Prim-Const and Prim-Fn, pi = omega
typeElim p _ii _g (Prim prim) =
  let arrow (x :| []) = VPrimTy x
      arrow (x :| (y : ys)) = VPi Omega (VPrimTy x) (const (pure (arrow (y :| ys))))
   in pure (Omega, arrow (Juvix.Core.Types.typeOf p prim))
-- App, function M applies to N (Elimination rule of dependent function types)
typeElim p ii g (App m n) = do
  mTy ← typeElim p ii g m -- annotation of M is usage sig and Pi with pi usage.
  case mTy of
    (sig, VPi pi varTy resultTy) → do
      (fst (writer (typeTerm p ii g n (sig <.> pi, varTy)))) -- N has to be of type varTy with usage sig*pi
      res ← resultTy =<< evalTerm p n []
      return (sig, res)
    _ →
      throw @"typecheckError" (MustBeFunction m ii n)
-- Conv
typeElim p ii g (Ann pi theTerm theType) =
  -- TODO check theType is of type Star first? But we have stakable universes now.
  -- typeTerm p ii g theType (pi, VStar 0) but if theType is function type then pi == 0 as per the *-Pi rule?
  do
    ty ← evalTerm p theType []
    fst $ writer $ typeTerm p ii g theTerm (pi, ty)
    return (pi, ty)
