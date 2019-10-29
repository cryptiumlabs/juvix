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
cEval ∷
  (Show primTy, Show primVal) ⇒
  Parameterisation primTy primVal →
  Term primTy primVal →
  Env primTy primVal →
  Value primTy primVal
cEval _ (Star i) _d = VStar i
cEval _ (PrimTy p) _d = VPrimTy p
cEval param (Pi pi ty ty') d =
  VPi pi (cEval param ty d) (\x → cEval param ty' (x : d))
cEval param (Lam e) d = VLam (\x → cEval param e (x : d))
cEval param (Elim ii) d = iEval param ii d

toInt ∷ Natural → Int
toInt = fromInteger . toInteger

--evaluation of inferable terms
-- TODO ∷ Promote iEval and cEval into the maybe monad and all call sites
iEval ∷
  (Show primTy, Show primVal) ⇒
  Parameterisation primTy primVal →
  Elim primTy primVal →
  Env primTy primVal →
  Value primTy primVal
iEval _ (Free x) _d = vfree x
iEval _ (Prim p) _d = VPrim p
iEval param (App iterm cterm) d =
  vapp param (iEval param iterm d) (cEval param cterm d)
iEval param (Ann _pi term _type) d = cEval param term d
iEval _ (Bound ii) d =
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
  case apply param x y of
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
cSubst ∷
  (Show primTy, Show primVal) ⇒
  Natural →
  Elim primTy primVal →
  Term primTy primVal →
  Term primTy primVal
cSubst _ii _r (Star i) = Star i
cSubst _ii _r (PrimTy p) = PrimTy p
cSubst ii r (Pi pi ty ty') = Pi pi (cSubst ii r ty) (cSubst (ii + 1) r ty')
cSubst ii r (Lam f) = Lam (cSubst (ii + 1) r f)
cSubst ii r (Elim e) = Elim (iSubst ii r e)

-- substitution function for inferable terms
iSubst ∷
  (Show primTy, Show primVal) ⇒
  Natural →
  Elim primTy primVal →
  Elim primTy primVal →
  Elim primTy primVal
iSubst ii r (Bound j)
  | ii == j = r
  | otherwise = Bound j
iSubst _ii _r (Free y) = Free y
iSubst _ii _r (Prim p) = Prim p
iSubst ii r (App it ct) = App (iSubst ii r it) (cSubst ii r ct)
iSubst ii r (Ann pi term t) = Ann pi (cSubst ii r term) (cSubst ii r t)

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
cType ∷
  (Show primTy, Show primVal, Eq primTy, Eq primVal) ⇒
  Parameterisation primTy primVal →
  Natural →
  Context primTy primVal →
  Term primTy primVal →
  Annotation primTy primVal →
  Result ()

-- *

cType _ _ii _g t@(Star n) ann = do
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
cType p ii g (Pi pi varType resultType) ann = do
  unless (SNat 0 == fst ann) (throwError "Sigma has to be 0.") -- checks sigma = 0.
  let ty = snd ann
  case ty of
    VStar _ → do
      cType p ii g varType ann -- checks varType is of type Star i
      let ty = cEval p varType []
      cType
        p -- checks resultType is of type Star i
        (ii + 1)
        ((Local ii, (pi, ty)) : g)
        (cSubst 0 (Free (Local ii)) resultType)
        ann
    _ →
      throwError
        "The variable type and the result type must be of type * at the same level."
-- primitive types are of type *0 with 0 usage (typing rule missing from lang ref?)
cType _ ii _g x@(PrimTy _) ann =
  unless
    (SNat 0 == fst ann && quote0 (snd ann) == Star 0)
    (throwError (errorMsg ii x (SNat 0, VStar 0) ann))
-- Lam (introduction rule of dependent function type)
cType p ii g (Lam s) ann =
  case ann of
    (sig, VPi pi ty ty') →
      -- Lam s should be of dependent function type (Pi pi ty ty').
      cType
        p
        (ii + 1)
        ((Local ii, (sig <.> pi, ty)) : g) -- put s in the context with usage sig*pi
        (cSubst 0 (Free (Local ii)) s) -- x (varType) in context S with sigma*pi usage.
        (sig, ty' (vfree (Local ii))) -- is of type M (usage sigma) in context T
    _ → throwError $ show (snd ann) <> " is not a function type but should be."
--
cType p ii g (Elim e) ann = do
  ann' ← iType p ii g e
  unless
    (fst ann == fst ann' && quote0 (snd ann) == quote0 (snd ann'))
    (throwError (errorMsg ii (Elim e) ann ann'))

-- inferable terms have type as output.
iType0 ∷
  (Show primTy, Show primVal, Eq primTy, Eq primVal) ⇒
  Parameterisation primTy primVal →
  Context primTy primVal →
  Elim primTy primVal →
  Result (Annotation primTy primVal)
iType0 p = iType p 0

iTypeErrorMsg ∷ Natural → Name → String
iTypeErrorMsg ii x =
  "Cannot find the type of \n" <> show x <> "\n (binder number " <> show ii
    <> ") in the environment."

iType ∷
  (Show primTy, Show primVal, Eq primTy, Eq primVal) ⇒
  Parameterisation primTy primVal →
  Natural →
  Context primTy primVal →
  Elim primTy primVal →
  Result (Annotation primTy primVal)
-- the type checker should never encounter a bound variable (as in LambdaPi)? To be confirmed.
iType _ _ii _g (Bound _) = error "Bound variable cannot be inferred"
iType _ ii g (Free x) =
  case lookup x g of
    Just ann → return ann
    Nothing → throwError (iTypeErrorMsg ii x)
-- Prim-Const and Prim-Fn, pi = omega
iType p _ii _g (Prim prim) =
  let arrow [x] = VPrimTy x
      arrow (x : xs) = VPi Omega (VPrimTy x) (const (arrow xs))
   in case typeOf p arrow prim of
        Right a → return (Omega, VPrimTy a)
        Left f → return (Omega, f)
-- App, function M applies to N (Elimination rule of dependent function types)
iType p ii g (App m n) = do
  mTy ← iType p ii g m -- annotation of M is usage sig and Pi with pi usage.
  case mTy of
    (sig, VPi pi varTy resultTy) → do
      cType p ii g n (sig <.> pi, varTy) -- N has to be of type varTy with usage sig*pi
      return (sig, resultTy (cEval p n []))
    _ →
      throwError
        ( show m <> "\n (binder number " <> show ii
            <> ") is not a function type and thus \n"
            <> show n
            <> "\n cannot be applied to it."
        )
-- Conv
iType p ii g (Ann pi theTerm theType) =
  -- TODO check theType is of type Star first? But we have stakable universes now.
  -- cType p ii g theType (pi, VStar 0) but if theType is function type then pi == 0 as per the *-Pi rule?
  do
    let ty = cEval p theType []
    cType p ii g theTerm (pi, ty)
    return (pi, ty)
