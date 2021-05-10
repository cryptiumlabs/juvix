{-# LANGUAGE DuplicateRecordFields #-}

module Juvix.Core.Unify.Types
  ( module Juvix.Core.Unify.Term,

    -- * Metavariables
    MetaVar,
    MetaSet,
    MetaMap,
    metasT,
    metasE,
    metasV,
    metasN,
    occursT,
    occursE,
    occursV,
    occursN,

    -- * Substitutions
    Subst (..),
    appSubstV,
    appSubstN,

    -- * Unification
    Unify (..),
    HasError,
    HasSubst,
    HasProblems,
    HasGlobals,
    CanUnify,
    PrimUnify,
    PrimUnify1,
    Problem,
    Error (..),
    runUnify,
    throwU,
    bind,
    occursCheck,
    nextProblem,
    addProblem,
    addProblems,

    -- * Reexports
    Universe,
    GlobalName,
    PatternVar,
    PatternMap,
    BoundVar,
    Name (..),
  )
where

import Data.Sequence (Seq (..))
import Juvix.Core.IR.Evaluator.Weak
import qualified Juvix.Core.IR.Types as IR
import Juvix.Core.IR.Types.Base
import Juvix.Core.Unify.MetaVar (MetaMap, MetaSet, MetaVar)
import qualified Juvix.Core.Unify.MetaVar as Meta
import Juvix.Core.Unify.Subst
import Juvix.Core.Unify.Term
import Juvix.Library
import qualified Juvix.Library.Usage as Usage

metasT :: Term primTy primVal -> MetaSet
metasT = \case
  Star _ℓ -> mempty
  PrimTy _t -> mempty
  Prim _p -> mempty
  Pi _π a b -> metasT a <> metasT b
  Lam t -> metasT t
  Sig _π a b -> metasT a <> metasT b
  Pair a b -> metasT a <> metasT b
  Let _π e b -> metasE e <> metasT b
  UnitTy -> mempty
  Unit -> mempty
  Elim e -> metasE e
  Meta α -> Meta.singleS α

metasE :: Elim primTy primVal -> MetaSet
metasE = \case
  Bound _i -> mempty
  Free _x -> mempty
  App f s -> metasE f <> metasT s
  Ann _π t a _ℓ -> metasT t <> metasT a

metasV :: Value primTy primVal -> MetaSet
metasV = \case
  VStar _ℓ -> mempty
  VPrimTy _t -> mempty
  VPrim _p -> mempty
  VPi _π a b -> metasV a <> metasV b
  VLam t -> metasV t
  VSig _π a b -> metasV a <> metasV b
  VPair a b -> metasV a <> metasV b
  VUnitTy -> mempty
  VUnit -> mempty
  VNeutral n -> metasN n
  VMeta α -> Meta.singleS α

metasN :: Neutral primTy primVal -> MetaSet
metasN = \case
  NBound _i -> mempty
  NFree _x -> mempty
  NApp f s -> metasN f <> metasV s

occursT :: MetaVar -> Term primTy primVal -> Bool
occursT α t = α `Meta.memberS` metasT t

occursE :: MetaVar -> Elim primTy primVal -> Bool
occursE α e = α `Meta.memberS` metasE e

occursV :: MetaVar -> Value primTy primVal -> Bool
occursV α t = α `Meta.memberS` metasV t

occursN :: MetaVar -> Neutral primTy primVal -> Bool
occursN α t = α `Meta.memberS` metasN t

data Env primTy primVal = Env
  { subst :: Subst primTy primVal,
    problems :: Seq (Problem primTy primVal),
    globals :: IR.Globals primTy primVal
  }
  deriving (Generic)

data Error primTy primVal
  = Occurs MetaVar (Value primTy primVal)
  | Clash (Value primTy primVal) (Value primTy primVal)
  | ClashU Usage.T Usage.T
  deriving (Eq, Show)

type Problem primTy primVal = (Value primTy primVal, Value primTy primVal)

type Problems primTy primVal = Seq (Problem primTy primVal)

type UnifyAlias primTy primVal =
  ExceptT (Error primTy primVal) (State (Env primTy primVal))

newtype Unify primTy primVal a = U (UnifyAlias primTy primVal a)
  deriving newtype (Functor, Applicative, Monad)
  deriving
    (HasThrow "unifyError" (Error primTy primVal))
    via MonadError (UnifyAlias primTy primVal)
  deriving
    ( HasSource "subst" (Subst primTy primVal),
      HasSink "subst" (Subst primTy primVal),
      HasState "subst" (Subst primTy primVal)
    )
    via StateField "subst" (UnifyAlias primTy primVal)
  deriving
    ( HasSource "problems" (Problems primTy primVal),
      HasSink "problems" (Problems primTy primVal),
      HasState "problems" (Problems primTy primVal)
    )
    via StateField "problems" (UnifyAlias primTy primVal)
  deriving
    ( HasSource "globals" (IR.Globals primTy primVal),
      HasReader "globals" (IR.Globals primTy primVal)
    )
    via ReaderField "globals" (UnifyAlias primTy primVal)

type HasError primTy primVal = HasThrow "unifyError" (Error primTy primVal)

type HasSubst primTy primVal = HasState "subst" (Subst primTy primVal)

type HasProblems primTy primVal = HasState "problems" (Problems primTy primVal)

type HasGlobals primTy primVal = HasReader "globals" (IR.Globals primTy primVal)

type CanUnify primTy primVal m =
  ( PrimUnify primTy primVal,
    HasError primTy primVal m,
    HasSubst primTy primVal m,
    HasProblems primTy primVal m,
    HasGlobals primTy primVal m
  )

type PrimUnify primTy primVal =
  (PrimUnify1 primTy, PrimUnify1 primVal)

type PrimUnify1 a = (Eq a, HasWeak a)

runUnify ::
  IR.Globals primTy primVal ->
  Unify primTy primVal a ->
  Either (Error primTy primVal) a
runUnify g (U m) = evalState (runExceptT m) st
  where
    st = Env {globals = g, subst = idSubst, problems = mempty}

throwU :: HasError primTy primVal m => Error primTy primVal -> m ()
throwU = throw @"unifyError"

bind ::
  ( HasSubst primTy primVal m,
    HasProblems primTy primVal m,
    HasWeak primTy,
    HasWeak primVal
  ) =>
  MetaVar ->
  Value primTy primVal ->
  m ()
bind α t = do
  modify @"subst" $ addSubst α t
  modify @"problems" $ fmap $ bimap (subst1 α t) (subst1 α t)

occursCheck ::
  HasError primTy primVal m =>
  MetaVar ->
  Value primTy primVal ->
  m ()
occursCheck α t = when (occursV α t) $ throwU $ Occurs α t

nextProblem ::
  HasProblems primTy primVal m => m (Maybe (Problem primTy primVal))
nextProblem = state @"problems" \case
  Empty -> (Nothing, Empty)
  p :<| ps -> (Just p, ps)

addProblems ::
  (HasProblems primTy primVal m, Foldable t) =>
  t (Problem primTy primVal) ->
  m ()
addProblems = traverse_ (uncurry addProblem)

addProblem ::
  (HasProblems primTy primVal m) =>
  Value primTy primVal ->
  Value primTy primVal ->
  m ()
addProblem s t = modify @"problems" (:|> (s, t))
