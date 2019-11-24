module Juvix.Core.IR.Typechecker where

import Control.Lens ((^?), ix)
import Juvix.Core.IR.Types
import Juvix.Core.Types
import Juvix.Core.Usage
import Juvix.Library hiding (show)
import Prelude (lookup)

--evaluation of checkable terms
evalTerm ∷
  ∀ primTy primVal m.
  ( HasThrow "typecheckError" (TypecheckError primTy primVal m) m,
    Show primTy,
    Show primVal
  ) ⇒
  Parameterisation primTy primVal →
  Term primTy primVal →
  Env primTy primVal m →
  m (Value primTy primVal m)
evalTerm _ (Star i) _d = pure (VStar i)
evalTerm _ (PrimTy p) _d = pure (VPrimTy p)
evalTerm param (Pi pi ty ty') d = do
  ty ← evalTerm param ty d
  pure (VPi pi ty (\x → evalTerm param ty' (x : d)))
evalTerm param (Lam e) d = pure (VLam (\x → evalTerm param e (x : d)))
evalTerm param (Elim ii) d = evalElim param ii d

toInt ∷ Natural → Int
toInt = fromInteger . toInteger

--evaluation of inferable terms
-- TODO ∷ Promote evalElim and evalTerm into the maybe monad and all call sites
evalElim ∷
  ∀ primTy primVal m.
  ( HasThrow "typecheckError" (TypecheckError primTy primVal m) m,
    Show primTy,
    Show primVal
  ) ⇒
  Parameterisation primTy primVal →
  Elim primTy primVal →
  Env primTy primVal m →
  m (Value primTy primVal m)
evalElim _ (Free x) _d = pure (vfree x)
evalElim _ (Prim p) _d = pure (VPrim p)
evalElim param (App elim term) d = do
  elim ← evalElim param elim d
  term ← evalTerm param term d
  vapp param elim term
evalElim param (Ann _pi term _type) d = evalTerm param term d
evalElim _ (Bound ii) d =
  fromMaybe (throw @"typecheckError" (UnboundIndex ii)) (pure |<< (d ^? ix (toInt ii)))

vapp ∷
  ∀ primTy primVal m.
  ( HasThrow "typecheckError" (TypecheckError primTy primVal m) m,
    Show primTy,
    Show primVal
  ) ⇒
  Parameterisation primTy primVal →
  Value primTy primVal m →
  Value primTy primVal m →
  m (Value primTy primVal m)
vapp _ (VLam f) v = f v
vapp _ (VNeutral n) v = pure (VNeutral (NApp n v))
vapp param (VPrim x) (VPrim y) =
  case Juvix.Core.Types.apply param x y of
    Just v → pure (VPrim v)
    Nothing → throw @"typecheckError" (CannotApply (VPrim x) (VPrim y))
vapp _ f x = throw @"typecheckError" (CannotApply f x)

-- substitution function for checkable terms
substTerm ∷
  (Show primTy, Show primVal) ⇒
  Natural →
  Elim primTy primVal →
  Term primTy primVal →
  Term primTy primVal
substTerm _ii _r (Star i) = Star i
substTerm _ii _r (PrimTy p) = PrimTy p
substTerm ii r (Pi pi ty ty') = Pi pi (substTerm ii r ty) (substTerm (ii + 1) r ty')
substTerm ii r (Lam f) = Lam (substTerm (ii + 1) r f)
substTerm ii r (Elim e) = Elim (substElim ii r e)

-- substitution function for inferable terms
substElim ∷
  (Show primTy, Show primVal) ⇒
  Natural →
  Elim primTy primVal →
  Elim primTy primVal →
  Elim primTy primVal
substElim ii r (Bound j)
  | ii == j = r
  | otherwise = Bound j
substElim _ii _r (Free y) = Free y
substElim _ii _r (Prim p) = Prim p
substElim ii r (App it ct) = App (substElim ii r it) (substTerm ii r ct)
substElim ii r (Ann pi term t) = Ann pi (substTerm ii r term) (substTerm ii r t)

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
  m ()

-- *

typeTerm _ _ii _g t@(Star n) ann = do
  unless (SNat 0 == fst ann) (throw @"typecheckError" SigmaMustBeZero) -- checks sigma = 0.
  let ty = snd ann
  case ty of
    VStar j →
      unless
        (n < j)
        (throw @"typecheckError" (UniverseMismatch t ty))
    _ → throw @"typecheckError" (ShouldBeStar (snd ann))
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
