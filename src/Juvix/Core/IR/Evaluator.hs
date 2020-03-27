{-# LANGUAGE UndecidableInstances #-}

-- |
-- This includes the evaluators (evalTerm and evalElim),
-- the value application function (vapp) and
-- the substitution functions (substTerm and substElim).
module Juvix.Core.IR.Evaluator where

import Control.Lens ((^?), ix)
import qualified Juvix.Core.Parameterisation as Param
import qualified Juvix.Core.IR.Types as IR
import qualified Juvix.Core.IR.Types.Base as IR
import qualified Juvix.Core.IR.Typechecker.Types as TC
import Juvix.Library hiding (show)
import Prelude (Show (..))


-- Evaluation environment, containing values for each bound variable
type Env primTy primVal m = [IR.Value primTy primVal m]

-- initial environment
initEnv ∷ Env primTy primVal m
initEnv = []

-- Type for dealing with evaling terms
type Eval termOrEval primTy primVal ext m =
  Param.Parameterisation primTy primVal ->
  termOrEval ext primTy primVal ->
  Env primTy primVal m ->
  m (IR.Value primTy primVal m)

-- Type that deals with TermX conversions ontop of eval terms!
type X primTy primVal ext m result =
  (IR.TermX ext primTy primVal -> m (IR.Value primTy primVal m)) ->
  (IR.ElimX ext primTy primVal -> m (IR.Value primTy primVal m)) ->
  result

type EvalTerm primTy primVal ext m =
  Eval IR.Term' primTy primVal ext m

type EvalTermX primTy primVal ext m =
  X primTy primVal ext m (EvalTerm primTy primVal ext m)

type EvalElim primTy primVal ext m =
  Eval IR.Elim' primTy primVal ext m

type EvalElimX primTy primVal ext m =
  X primTy primVal ext m (EvalElim primTy primVal ext m)

-- evaluation of checkable terms
evalTermWith ::
  ( HasThrow "typecheckError" (TC.TypecheckError primTy primVal m) m,
    Show primTy,
    Show primVal
  ) =>
  EvalTermX primTy primVal ext m
evalTermWith tx ex param p d =
  case p of
    IR.Star' ist _ -> pure (IR.VStar ist)
    IR.PrimTy' p _ -> pure (IR.VPrimTy p)
    IR.Elim' ist _ -> evalElimWith tx ex param ist d
    IR.TermX exprs -> tx exprs
    IR.Pi' pi ty ty' _ -> do
      ty <- evalTermWith tx ex param ty d
      pure (IR.VPi pi ty (\x -> evalTermWith tx ex param ty' (x : d)))
    IR.Lam' e _ ->
      pure (IR.VLam (\x -> evalTermWith tx ex param e (x : d)))

evalTerm ::
  ( HasThrow "typecheckError" (TC.TypecheckError primTy primVal m) m,
    Show primTy,
    Show primVal,
    IR.TermX ext primTy primVal ~ Void,
    IR.ElimX ext primTy primVal ~ Void
  ) =>
  EvalTerm primTy primVal ext m
evalTerm = evalTermWith absurd absurd

-- evaluation of inferable terms
evalElimWith ::
  ( HasThrow "typecheckError" (TC.TypecheckError primTy primVal m) m,
    Show primTy,
    Show primVal
  ) =>
  EvalElimX primTy primVal ext m
evalElimWith tx ex param p d =
  case p of
    IR.Free' x _ -> pure (IR.vfree x)
    IR.Prim' p _ -> pure (IR.VPrim p)
    IR.ElimX val -> ex val
    IR.Ann' _pi term _type _ ->
      evalTermWith tx ex param term d
    IR.App' elim term _ -> do
      elim <- evalElimWith tx ex param elim d
      term <- evalTermWith tx ex param term d
      vapp param elim term
    IR.Bound' ii _ ->
      maybe (throw @"typecheckError" (TC.UnboundIndex ii)) pure $
        d ^? ix (fromIntegral ii)

evalElim ::
  ( HasThrow "typecheckError" (TC.TypecheckError primTy primVal m) m,
    Show primTy,
    Show primVal,
    IR.TermX ext primTy primVal ~ Void,
    IR.ElimX ext primTy primVal ~ Void
  ) =>
  EvalElim primTy primVal ext m
evalElim = evalElimWith absurd absurd

-- value application function
vapp ::
  ( HasThrow "typecheckError" (TC.TypecheckError primTy primVal m) m,
    Show primTy,
    Show primVal
  ) =>
  Param.Parameterisation primTy primVal ->
  IR.Value primTy primVal m ->
  IR.Value primTy primVal m ->
  m (IR.Value primTy primVal m)
vapp _ (IR.VLam f) v = f v
vapp _ (IR.VNeutral n) v = pure (IR.VNeutral (IR.NApp n v))
vapp param (IR.VPrim x) (IR.VPrim y) =
  case Param.apply param x y of
    Just v → pure (IR.VPrim v)
    Nothing → throw @"typecheckError" (TC.CannotApply (IR.VPrim x) (IR.VPrim y))
vapp _ f x = throw @"typecheckError" (TC.CannotApply f x)

class CanSubst ext primTy primVal t where
  subst :: Natural -> IR.Elim' ext primTy primVal -> t -> t

instance CanSubst ext primTy primVal () where subst _ _ = identity

instance CanSubst ext primTy primVal Void where subst _ _ = absurd

instance
  ( IR.TermAll (CanSubst ext primTy primVal) ext primTy primVal,
    IR.ElimAll (CanSubst ext primTy primVal) ext primTy primVal
  ) =>
  CanSubst ext primTy primVal (IR.Term' ext primTy primVal)
  where
  subst ii r (IR.Star' i a) = IR.Star' i (subst ii r a)
  subst ii r (IR.PrimTy' p a) = IR.PrimTy' p (subst ii r a)
  subst ii r (IR.Pi' pi ty ty' a) =
    IR.Pi' pi (subst ii r ty) (subst (ii + 1) r ty') (subst ii r a)
  subst ii r (IR.Lam' f a) = IR.Lam' (subst (ii + 1) r f) (subst ii r a)
  subst ii r (IR.Elim' e a) = IR.Elim' (subst ii r e) (subst ii r a)
  subst ii r (IR.TermX a) = IR.TermX (subst ii r a)

instance
  ( IR.TermAll (CanSubst ext primTy primVal) ext primTy primVal,
    IR.ElimAll (CanSubst ext primTy primVal) ext primTy primVal
  ) =>
  CanSubst ext primTy primVal (IR.Elim' ext primTy primVal)
  where
  subst ii r (IR.Bound' j a)
    | ii == j = r
    | otherwise = IR.Bound' j (subst ii r a)
  subst ii r (IR.Free' x a) = IR.Free' x (subst ii r a)
  subst ii r (IR.Prim' k a) = IR.Prim' k (subst ii r a)
  subst ii r (IR.App' it ct a) =
    IR.App' (subst ii r it) (subst ii r ct) (subst ii r a)
  subst ii r (IR.Ann' pi term t a) =
    IR.Ann' pi (subst ii r term) (subst ii r t) (subst ii r a)
  subst ii r (IR.ElimX a) = IR.ElimX (subst ii r a)

substTerm ::
  ( IR.TermAll (CanSubst ext primTy primVal) ext primTy primVal,
    IR.ElimAll (CanSubst ext primTy primVal) ext primTy primVal
  ) =>
  Natural ->
  IR.Elim' ext primTy primVal ->
  IR.Term' ext primTy primVal ->
  IR.Term' ext primTy primVal
substTerm = subst

substElim ::
  ( IR.TermAll (CanSubst ext primTy primVal) ext primTy primVal,
    IR.ElimAll (CanSubst ext primTy primVal) ext primTy primVal
  ) =>
  Natural ->
  IR.Elim' ext primTy primVal ->
  IR.Elim' ext primTy primVal ->
  IR.Elim' ext primTy primVal
substElim = subst
