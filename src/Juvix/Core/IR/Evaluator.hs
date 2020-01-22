{-# LANGUAGE UndecidableInstances #-} -- for the 'CanSubst' instances

-- |
-- This includes the evaluators (evalTerm and evalElim),
-- the value application function (vapp) and
-- the substitution functions (substTerm and substElim).
module Juvix.Core.IR.Evaluator where

import Control.Lens ((^?), ix)
import Juvix.Core.IR.Types
import Juvix.Core.Types
import Juvix.Library hiding (show)

-- TODO: extend functions to annotated terms:
-- - eval*
-- - subst*

-- evaluation of checkable terms
evalTermWith ∷
  ∀ ext primTy primVal m.
  ( HasThrow "typecheckError" (TypecheckError primTy primVal m) m,
    Show primTy,
    Show primVal
  ) ⇒
  (TermX ext primTy primVal → m (Value primTy primVal m)) →
  (ElimX ext primTy primVal → m (Value primTy primVal m)) →
  Parameterisation primTy primVal →
  Term' ext primTy primVal →
  Env primTy primVal m →
  m (Value primTy primVal m)
evalTermWith _ _ _ (Star' i _) _d = pure (VStar i)
evalTermWith _ _ _ (PrimTy' p _) _d = pure (VPrimTy p)
evalTermWith tx ex param (Pi' pi ty ty' _) d = do
  ty ← evalTermWith tx ex param ty d
  pure (VPi pi ty (\x → evalTermWith tx ex param ty' (x : d)))
evalTermWith tx ex param (Lam' e _) d =
  pure (VLam (\x → evalTermWith tx ex param e (x : d)))
evalTermWith tx ex param (Elim' ii _) d = evalElimWith tx ex param ii d
evalTermWith tx _ _ (TermX e) _ = tx e

evalTerm ∷
  ∀ ext primTy primVal m.
  ( HasThrow "typecheckError" (TypecheckError primTy primVal m) m,
    Show primTy,
    Show primVal,
    TermX ext primTy primVal ~ Void,
    ElimX ext primTy primVal ~ Void
  ) ⇒
  Parameterisation primTy primVal →
  Term' ext primTy primVal →
  Env primTy primVal m →
  m (Value primTy primVal m)
evalTerm = evalTermWith absurd absurd

-- evaluation of inferable terms
evalElimWith ∷
  ∀ ext primTy primVal m.
  ( HasThrow "typecheckError" (TypecheckError primTy primVal m) m,
    Show primTy,
    Show primVal
  ) ⇒
  (TermX ext primTy primVal → m (Value primTy primVal m)) →
  (ElimX ext primTy primVal → m (Value primTy primVal m)) →
  Parameterisation primTy primVal →
  Elim' ext primTy primVal →
  Env primTy primVal m →
  m (Value primTy primVal m)
evalElimWith _ _ _ (Free' x _) _d = pure (vfree x)
evalElimWith _ _ _ (Prim' p _) _d = pure (VPrim p)
evalElimWith tx ex param (App' elim term _) d = do
  elim ← evalElimWith tx ex param elim d
  term ← evalTermWith tx ex param term d
  vapp param elim term
evalElimWith tx ex param (Ann' _pi term _type _) d =
  evalTermWith tx ex param term d
evalElimWith _ _ _ (Bound' ii _) d =
  fromMaybe (throw @"typecheckError" (UnboundIndex ii)) $
    pure |<< (d ^? ix (fromIntegral ii))
evalElimWith _ ex _ (ElimX v) _ = ex v

evalElim ∷
  ∀ ext primTy primVal m.
  ( HasThrow "typecheckError" (TypecheckError primTy primVal m) m,
    Show primTy,
    Show primVal,
    TermX ext primTy primVal ~ Void,
    ElimX ext primTy primVal ~ Void
  ) ⇒
  Parameterisation primTy primVal →
  Elim' ext primTy primVal →
  Env primTy primVal m →
  m (Value primTy primVal m)
evalElim = evalElimWith absurd absurd

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


class CanSubst ext primTy primVal t where
  subst :: Natural
        -> Elim' ext primTy primVal
        -> t
        -> t

instance CanSubst ext primTy primVal ()   where subst _ _ = identity
instance CanSubst ext primTy primVal Void where subst _ _ = absurd

instance TEAll (CanSubst ext primTy primVal) ext primTy primVal =>
  CanSubst ext primTy primVal (Term' ext primTy primVal)
 where
  subst ii r (Star' i a) = Star' i (subst ii r a)
  subst ii r (PrimTy' p a) = PrimTy' p (subst ii r a)
  subst ii r (Pi' pi ty ty' a) =
    Pi' pi (subst ii r ty) (subst (ii + 1) r ty') (subst ii r a)
  subst ii r (Lam' f a) = Lam' (subst (ii + 1) r f) (subst ii r a)
  subst ii r (Elim' e a) = Elim' (subst ii r e) (subst ii r a)
  subst ii r (TermX a) = TermX (subst ii r a)

instance TEAll (CanSubst ext primTy primVal) ext primTy primVal =>
  CanSubst ext primTy primVal (Elim' ext primTy primVal)
 where
  subst ii r (Bound' j a)
    | ii == j = r
    | otherwise = Bound' j (subst ii r a)
  subst ii r (Free' x a) = Free' x (subst ii r a)
  subst ii r (Prim' k a) = Prim' k (subst ii r a)
  subst ii r (App' it ct a) =
    App' (subst ii r it) (subst ii r ct) (subst ii r a)
  subst ii r (Ann' pi term t a) =
    Ann' pi (subst ii r term) (subst ii r t) (subst ii r a)
  subst ii r (ElimX a) = ElimX (subst ii r a)


substTerm ∷ TEAll (CanSubst ext primTy primVal) ext primTy primVal =>
  Natural → Elim' ext primTy primVal →
  Term' ext primTy primVal → Term' ext primTy primVal
substTerm = subst

substElim ∷ TEAll (CanSubst ext primTy primVal) ext primTy primVal =>
  Natural → Elim' ext primTy primVal →
  Elim' ext primTy primVal → Elim' ext primTy primVal
substElim = subst

