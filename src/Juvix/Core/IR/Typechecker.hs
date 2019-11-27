module Juvix.Core.IR.Typechecker where

import Control.Monad.Writer as W
import Juvix.Core.IR.Evaluator
import Juvix.Core.IR.Types
import Juvix.Core.Types
import Juvix.Core.Usage
import Juvix.Library hiding (show)
import Prelude (String, lookup)

-- | 'checker' for checkable terms checks the term against an annotation and returns ().
typeTerm ∷
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
  Term primTy primVal →
  Annotation primTy primVal m →
  Writer [LogEntry] (m ())

data LogEntry = LogEntry {msg ∷ String}
  deriving (Eq, Show)

logOutput ∷ String → Writer [LogEntry] ()
logOutput s = W.tell [LogEntry s]

-- * (Universe formation rule)

typeTerm _ _ii _g t@(Star i) ann = do
  logOutput "Checking that Sigma is zero"
  unless (SNat 0 == fst ann) (throw @"typecheckError" SigmaMustBeZero) -- checks sigma = 0.
  let ty = snd ann
  logOutput "Checking that the annotation is of type *j, and j>i "
  case ty of
    VStar j →
      if (i >= j)
        then
          ( do
              logOutput "j is not greater then"
              return (throw @"typecheckError" (UniverseMismatch t ty))
          )
        else return (return ()) -- TODO use Monad transformer? Not sure why this unless doesn't work.
    _ → do
      logOutput " The annotation is not of type *."
      return (throw @"typecheckError" (ShouldBeStar (snd ann)))
typeTerm p ii g (Pi pi varType resultType) ann = do
  unless (SNat 0 == fst ann) (throw @"typecheckError" SigmaMustBeZero) -- checks sigma = 0.
  case snd ann of
    VStar _ → do
      typeTerm p ii g varType ann -- checks varType is of type Star i
      ty ← evalTerm p varType []
      typeTerm
        p -- checks resultType is of type Star i
        (ii + 1)
        ((Local ii, (pi, ty)) : g)
        (substTerm 0 (Free (Local ii)) resultType)
        ann
    _ →
      throw @"typecheckError" UniverseLevelMustMatch
-- primitive types are of type *0 with 0 usage (typing rule missing from lang ref?)
typeTerm _ ii _g x@(PrimTy _) ann = do
  ty ← quote0 (snd ann)
  unless
    (SNat 0 == fst ann && ty == Star 0)
    (throw @"typecheckError" (TypeMismatch ii x (SNat 0, VStar 0) ann))
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
  unless
    (fst ann' `allowsUsageOf` fst ann && annt == annt')
    (throw @"typecheckError" (TypeMismatch ii (Elim e) ann ann'))

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
      typeTerm p ii g n (sig <.> pi, varTy) -- N has to be of type varTy with usage sig*pi
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
    typeTerm p ii g theTerm (pi, ty)
    return (pi, ty)
