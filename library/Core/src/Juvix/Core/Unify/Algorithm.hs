module Juvix.Core.Unify.Algorithm
  ( module Juvix.Core.Unify.Types,
    UnifyResult,
    Success (..),
    unifyV,
    unifyN,
  )
where

import qualified Data.HashMap.Strict as HashMap
import Juvix.Core.IR.Evaluator.Weak
import qualified Juvix.Core.IR.Types as IR
import qualified Juvix.Core.Unify.MetaVar as Meta
import Juvix.Core.Unify.Types
import Juvix.Library
import qualified Juvix.Library.Usage as Usage

type UnifyResult primTy primVal =
  Either (Error primTy primVal) (Success primTy primVal)

data Success primTy primVal = Success
  { subst :: Subst primTy primVal,
    unsolved :: MetaSet
  }

unifyV ::
  PrimUnify primTy primVal =>
  IR.Globals primTy primVal ->
  Value primTy primVal ->
  Value primTy primVal ->
  UnifyResult primTy primVal
unifyV globals s t = runUnify globals do
  addProblem s t
  doUnify
  Success <$> get @"subst" <*> getUnsolved s t

unifyN ::
  PrimUnify primTy primVal =>
  IR.Globals primTy primVal ->
  Neutral primTy primVal ->
  Neutral primTy primVal ->
  UnifyResult primTy primVal
unifyN globals = unifyV globals `on` VNeutral

doUnify :: CanUnify primTy primVal m => m ()
doUnify =
  nextProblem >>= \case
    Just (s, t) -> unify1 s t >> doUnify
    Nothing -> pure ()

-- FIXME make adjustments when going under binders
unify1 ::
  CanUnify primTy primVal m =>
  Value primTy primVal ->
  Value primTy primVal ->
  m ()
unify1 (VMeta α) (VMeta β) = unless (α == β) $ bind α (VMeta β)
unify1 (VMeta α) t = tryBind α t
unify1 s (VMeta α) = tryBind α s
unify1 s@(VStar 𝓀) t@(VStar ℓ) = eqAtom 𝓀 ℓ s t
unify1 s@(VPrimTy a) t@(VPrimTy b) = eqAtom a b s t
unify1 (VPi π a b) (VPi ρ c d) = unifyBinder (π, a, b) (ρ, c, d)
unify1 (VLam s) (VLam t) = addProblem s t
unify1 (VSig π a b) (VSig ρ c d) = unifyBinder (π, a, b) (ρ, c, d)
unify1 (VPair s t) (VPair u v) = addProblems [(s, t), (u, v)]
unify1 VUnitTy VUnitTy = pure ()
unify1 VUnit VUnit = pure ()
unify1 (VNeutral e) (VNeutral f) = unify1N e f
unify1 s@(VPrim p) t@(VPrim q) = eqAtom p q s t
unify1 s t = clash s t

unify1N ::
  CanUnify primTy primVal m =>
  Neutral primTy primVal ->
  Neutral primTy primVal ->
  m ()
unify1N (NApps f ss) (NApps g ts) = unifyApp f g ss ts
unify1N e@(NBound i) f@(NBound j) = eqAtomN i j e f
unify1N e@(NFree x) f@(NFree y) = eqAtomN x y e f
unify1N e f = clashN e f

tryBind ::
  ( HasWeak primTy,
    HasWeak primVal,
    HasError primTy primVal m,
    HasProblems primTy primVal m,
    HasSubst primTy primVal m
  ) =>
  MetaVar ->
  Value primTy primVal ->
  m ()
tryBind α t = occursCheck α t >> bind α t

unifyApp ::
  ( Eq primTy,
    Eq primVal,
    HasError primTy primVal m,
    HasGlobals primTy primVal m,
    HasProblems primTy primVal m
  ) =>
  Neutral primTy primVal ->
  Neutral primTy primVal ->
  [Value primTy primVal] ->
  [Value primTy primVal] ->
  m ()
unifyApp f g ss ts = do
  eqNeut f g
  inj <- isInjective f
  let unif' = if inj then addProblem else eqVal
  sequence_ $ zipWith unif' ss ts

unifyBinder ::
  (HasError primTy primVal m, HasProblems primTy primVal m) =>
  (Usage.T, Value primTy primVal, Value primTy primVal) ->
  (Usage.T, Value primTy primVal, Value primTy primVal) ->
  m ()
unifyBinder (π, a, b) (ρ, c, d) = do
  eqUsage π ρ
  addProblems [(a, c), (b, d)]

isInjective :: HasGlobals primTy primVal m => Neutral primTy primVal -> m Bool
isInjective (NFree (Global x)) =
  asks @"globals" (HashMap.lookup x) >>| \case
    Just (IR.GDataCon _) -> True
    _ -> False
isInjective _ = pure False

getUnsolved ::
  (HasSubst primTy primVal m, HasProblems primTy primVal m) =>
  Value primTy primVal ->
  Value primTy primVal ->
  m MetaSet
getUnsolved s t = do
  sub <- gets @"subst" getSubst
  probs <- get @"problems"
  let go = Meta.filterS (not . (`Meta.memberM` sub)) . metasV
  let unsS = foldMap go sub
  let unsP = foldMap (\(p, q) -> go p <> go q) probs
  pure $ go s <> go t <> unsS <> unsP

eqVal ::
  (Eq primTy, Eq primVal, HasError primTy primVal m) =>
  Value primTy primVal ->
  Value primTy primVal ->
  m ()
eqVal s t = unless (s == t) $ clash s t

eqNeut ::
  (Eq primTy, Eq primVal, HasError primTy primVal m) =>
  Neutral primTy primVal ->
  Neutral primTy primVal ->
  m ()
eqNeut = eqVal `on` VNeutral

eqUsage :: HasError primTy primVal m => Usage.T -> Usage.T -> m ()
eqUsage π ρ = unless (π == ρ) $ clashU π ρ

eqAtom ::
  (Eq a, HasError primTy primVal m) =>
  a ->
  a ->
  Value primTy primVal ->
  Value primTy primVal ->
  m ()
eqAtom x y s t = unless (x == y) $ clash s t

eqAtomN ::
  (Eq a, HasError primTy primVal m) =>
  a ->
  a ->
  Neutral primTy primVal ->
  Neutral primTy primVal ->
  m ()
eqAtomN x y = eqAtom x y `on` VNeutral

clash ::
  HasError primTy primVal m =>
  Value primTy primVal ->
  Value primTy primVal ->
  m ()
clash s t = throwU $ Clash s t

clashN ::
  HasError primTy primVal m =>
  Neutral primTy primVal ->
  Neutral primTy primVal ->
  m ()
clashN = clash `on` VNeutral

clashU :: HasError primTy primVal m => Usage.T -> Usage.T -> m ()
clashU π ρ = throwU $ ClashU π ρ
