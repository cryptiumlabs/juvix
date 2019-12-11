-- |
-- This includes the evaluators (evalTerm and evalElim),
-- the value application function (vapp) and
-- the substitution functions (substTerm and substElim).
module Juvix.Core.IR.Evaluator where

import Control.Lens ((^?), ix)
import Juvix.Core.IR.Types
import Juvix.Core.Types
import Juvix.Library hiding (show)

-- evaluation of checkable terms
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

-- evaluation of inferable terms
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

-- value application function
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
