module Juvix.Core.Erasure.Types where

import qualified Juvix.Core.Erased as Erased
import qualified Juvix.Core.IR.Types as IR
import Juvix.Library hiding (empty)

data Env primTy primVal
  = Env
      { typeAssignment ∷ Erased.TypeAssignment primTy,
        context ∷ IR.Context primTy primVal,
        nextName ∷ Int,
        nameStack ∷ [Int]
      }
  deriving (Show, Eq, Generic)

newtype EnvErasure primTy primVal a = EnvEra (ExceptT ErasureError (State (Env primTy primVal)) a)
  deriving (Functor, Applicative, Monad)
  deriving
    (HasState "typeAssignment" (Erased.TypeAssignment primTy))
    via Field "typeAssignment" () (MonadState (ExceptT ErasureError (State (Env primTy primVal))))
  deriving
    (HasState "context" (IR.Context primTy primVal))
    via Field "context" () (MonadState (ExceptT ErasureError (State (Env primTy primVal))))
  deriving
    (HasState "nextName" Int)
    via Field "nextName" () (MonadState (ExceptT ErasureError (State (Env primTy primVal))))
  deriving
    (HasState "nameStack" [Int])
    via Field "nameStack" () (MonadState (ExceptT ErasureError (State (Env primTy primVal))))
  deriving
    (HasThrow "erasureError" ErasureError)
    via MonadError (ExceptT ErasureError (MonadState (State (Env primTy primVal))))

data ErasureError
  = Unsupported
  | CannotEraseZeroUsageTerm Text
  | InternalError Text
  deriving (Show, Eq, Generic)
