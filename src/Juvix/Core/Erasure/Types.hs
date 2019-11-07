module Juvix.Core.Erasure.Types where

import qualified Juvix.Core.Erased as Erased
import Juvix.Library hiding (empty)
import Juvix.Utility

data Env primTy
  = Env
      { typeAssignment ∷ Erased.TypeAssignment primTy,
        nextName ∷ Int,
        nameStack ∷ [Int]
      }
  deriving (Show, Eq, Generic)

newtype EnvErasure primTy a = EnvEra (ExceptT ErasureError (State (Env primTy)) a)
  deriving (Functor, Applicative, Monad)
  deriving
    (HasState "typeAssignment" (Erased.TypeAssignment primTy))
    via Field "typeAssignment" () (MonadState (ExceptT ErasureError (State (Env primTy))))
  deriving
    (HasState "nextName" Int)
    via Field "nextName" () (MonadState (ExceptT ErasureError (State (Env primTy))))
  deriving
    (HasState "nameStack" [Int])
    via Field "nameStack" () (MonadState (ExceptT ErasureError (State (Env primTy))))
  deriving
    (HasThrow "erasureError" ErasureError)
    via MonadError (ExceptT ErasureError (MonadState (State (Env primTy))))

data ErasureError
  = Unsupported
  deriving (Show, Eq, Generic)
