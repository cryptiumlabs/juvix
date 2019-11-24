module Juvix.Core.IR.Typechecker where

import Control.Lens ((^?), ix)
import Control.Monad.Except (throwError)
import Juvix.Core.IR.Types
import Juvix.Core.Types
import Juvix.Core.Usage
import Juvix.Library hiding (show)
import Prelude (Show (..), String, error, lookup)

-- initial environment
initEnv ∷ Env primTy primVal
initEnv = []

--evaluation of checkable terms
evalTerm ∷
  (Show primTy, Show primVal) ⇒
  Parameterisation primTy primVal →
  Term primTy primVal →
  Env primTy primVal →
  Value primTy primVal
evalTerm _ (Star i) _d = VStar i
evalTerm _ (PrimTy p) _d = VPrimTy p
evalTerm param (Pi pi ty ty') d =
  VPi pi (evalTerm param ty d) (\x → evalTerm param ty' (x : d))
evalTerm param (Lam e) d = VLam (\x → evalTerm param e (x : d))
evalTerm param (Elim ii) d = evalElim param ii d

toInt ∷ Natural → Int
toInt = fromInteger . toInteger

--evaluation of inferable terms
-- TODO ∷ Promote evalElim and evalTerm into the maybe monad and all call sites
evalElim ∷
  (Show primTy, Show primVal) ⇒
  Parameterisation primTy primVal →
  Elim primTy primVal →
  Env primTy primVal →
  Value primTy primVal
evalElim _ (Free x) _d = vfree x
evalElim _ (Prim p) _d = VPrim p
evalElim param (App iterm cterm) d =
  vapp param (evalElim param iterm d) (evalTerm param cterm d)
evalElim param (Ann _pi term _type) d = evalTerm param term d
evalElim _ (Bound ii) d =
  fromMaybe (error ("unbound index " <> show ii)) (d ^? ix (toInt ii))

vapp ∷
  (Show primTy, Show primVal) ⇒
  Parameterisation primTy primVal →
  Value primTy primVal →
  Value primTy primVal →
  Value primTy primVal
vapp _ (VLam f) v = f v
vapp _ (VNeutral n) v = VNeutral (NApp n v)
vapp param (VPrim x) (VPrim y) =
  case Juvix.Core.Types.apply param x y of
    Just v → VPrim v
    Nothing →
      error
        ( "Primitive application error: cannot apply " <> show x <> " to "
            <> show y
        )
vapp _ x y =
  error
    ( "Application (vapp) error. Cannot apply \n" <> show y <> "\n to \n"
        <> show x
    )

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

-- error message for inferring/checking types
errorMsg ∷
  (Show primTy, Show primVal) ⇒
  Natural →
  Term primTy primVal →
  Annotation primTy primVal →
  Annotation primTy primVal →
  String
errorMsg binder cterm expectedT gotT =
  "Type mismatched. \n" <> show cterm <> " \n (binder number " <> show binder
    <> ") is of type \n"
    <> show (show (snd gotT))
    <> " , with "
    <> show (fst gotT)
    <> " usage.\n But the expected type is "
    <> show (show (snd expectedT))
    <> " , with "
    <> show (fst expectedT)
    <> " usage."

-- Type (and usage) checking
type Result a = Either String a --when type checking fails, it throws an error.

-- | 'checker' for checkable terms checks the term against an annotation and returns ().
typeTerm ∷
  (Show primTy, Show primVal, Eq primTy, Eq primVal) ⇒
  Parameterisation primTy primVal →
  Natural →
  Context primTy primVal →
  Term primTy primVal →
  Annotation primTy primVal →
  Result ()

-- *

typeTerm _ _ii _g t@(Star n) ann = do
  unless (SNat 0 == fst ann) (throwError "Sigma has to be 0.") -- checks sigma = 0.
  let ty = snd ann
  case ty of
    VStar j →
      unless
        (n < j)
        ( throwError $
            show t <> " is of type * of a higher universe. But the expected type "
              <> show (snd ann)
              <> " is * of a equal or lower universe."
        )
    _ → throwError $ "* n is of type * but " <> show (snd ann) <> " is not *."
typeTerm p ii g (Pi pi varType resultType) ann = do
  unless (SNat 0 == fst ann) (throwError "Sigma has to be 0.") -- checks sigma = 0.
  let ty = snd ann
  case ty of
    VStar _ → do
      typeTerm p ii g varType ann -- checks varType is of type Star i
      let ty = evalTerm p varType []
      typeTerm
        p -- checks resultType is of type Star i
        (ii + 1)
        ((Local ii, (pi, ty)) : g)
        (substTerm 0 (Free (Local ii)) resultType)
        ann
    _ →
      throwError
        "The variable type and the result type must be of type * at the same level."
-- primitive types are of type *0 with 0 usage (typing rule missing from lang ref?)
typeTerm _ ii _g x@(PrimTy _) ann =
  unless
    (SNat 0 == fst ann && quote0 (snd ann) == Star 0)
    (throwError (errorMsg ii x (SNat 0, VStar 0) ann))
-- Lam (introduction rule of dependent function type)
typeTerm p ii g (Lam s) ann =
  case ann of
    (sig, VPi pi ty ty') →
      -- Lam s should be of dependent function type (Pi pi ty ty').
      typeTerm
        p
        (ii + 1)
        ((Local ii, (sig <.> pi, ty)) : g) -- put s in the context with usage sig*pi
        (substTerm 0 (Free (Local ii)) s) -- x (varType) in context S with sigma*pi usage.
        (sig, ty' (vfree (Local ii))) -- is of type M (usage sigma) in context T
    _ → throwError $ show (snd ann) <> " is not a function type but should be - while checking " <> show (Lam s)
--
typeTerm p ii g (Elim e) ann = do
  ann' ← typeElim p ii g e
  unless
    (fst ann' `allowsUsageOf` fst ann && quote0 (snd ann) == quote0 (snd ann'))
    (throwError (errorMsg ii (Elim e) ann ann'))

-- inferable terms have type as output.
typeElim0 ∷
  (Show primTy, Show primVal, Eq primTy, Eq primVal) ⇒
  Parameterisation primTy primVal →
  Context primTy primVal →
  Elim primTy primVal →
  Result (Annotation primTy primVal)
typeElim0 p = typeElim p 0

typeElimErrorMsg ∷ Natural → Name → String
typeElimErrorMsg ii x =
  "Cannot find the type of \n" <> show x <> "\n (binder number " <> show ii
    <> ") in the environment."

typeElim ∷
  (Show primTy, Show primVal, Eq primTy, Eq primVal) ⇒
  Parameterisation primTy primVal →
  Natural →
  Context primTy primVal →
  Elim primTy primVal →
  Result (Annotation primTy primVal)
-- the type checker should never encounter a bound variable (as in LambdaPi)? To be confirmed.
typeElim _ _ii _g (Bound _) = error "Bound variable cannot be inferred"
typeElim _ ii g (Free x) =
  case lookup x g of
    Just ann → return ann
    Nothing → throwError (typeElimErrorMsg ii x)
-- Prim-Const and Prim-Fn, pi = omega
typeElim p _ii _g (Prim prim) =
  let arrow (x :| []) = VPrimTy x
      arrow (x :| (y : ys)) = VPi Omega (VPrimTy x) (const (arrow (y :| ys)))
   in pure (Omega, arrow (Juvix.Core.Types.typeOf p prim))
-- App, function M applies to N (Elimination rule of dependent function types)
typeElim p ii g (App m n) = do
  mTy ← typeElim p ii g m -- annotation of M is usage sig and Pi with pi usage.
  case mTy of
    (sig, VPi pi varTy resultTy) → do
      typeTerm p ii g n (sig <.> pi, varTy) -- N has to be of type varTy with usage sig*pi
      return (sig, resultTy (evalTerm p n []))
    _ →
      throwError
        ( show m <> "\n (binder number " <> show ii
            <> ") is not a function type and thus \n"
            <> show n
            <> "\n cannot be applied to it."
        )
-- Conv
typeElim p ii g (Ann pi theTerm theType) =
  -- TODO check theType is of type Star first? But we have stakable universes now.
  -- typeTerm p ii g theType (pi, VStar 0) but if theType is function type then pi == 0 as per the *-Pi rule?
  do
    let ty = evalTerm p theType []
    typeTerm p ii g theTerm (pi, ty)
    return (pi, ty)
