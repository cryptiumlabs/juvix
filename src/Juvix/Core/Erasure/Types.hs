module Juvix.Core.Erasure.Types where

import qualified Juvix.Core.Erased as Erased
import qualified Juvix.Core.IR.Typechecker.Types as TC
import Juvix.Library hiding (empty)

data Env primTy primVal
  = Env
      { typeAssignment :: Erased.TypeAssignment primTy
      , context :: TC.Context primTy primVal (TC.EnvTypecheck primTy primVal)
      , nextName :: Int
      , nameStack :: [Int]
      }
  deriving (Show, Eq, Generic)

newtype EnvT primTy primVal a
  = EnvEra (ExceptT Error (State (Env primTy primVal)) a)
  deriving (Functor, Applicative, Monad)
  deriving
    ( HasState "typeAssignment" (Erased.TypeAssignment primTy),
      HasSink "typeAssignment" (Erased.TypeAssignment primTy),
      HasSource "typeAssignment" (Erased.TypeAssignment primTy)
    )
    via Field "typeAssignment" () (MonadState (ExceptT Error (State (Env primTy primVal))))
  deriving
    ( HasState "context" (TC.Context primTy primVal (TC.EnvTypecheck primTy primVal)),
      HasSink "context" (TC.Context primTy primVal (TC.EnvTypecheck primTy primVal)),
      HasSource "context" (TC.Context primTy primVal (TC.EnvTypecheck primTy primVal))
    )
    via Field "context" () (MonadState (ExceptT Error (State (Env primTy primVal))))
  deriving
    ( HasState "nextName" Int,
      HasSink "nextName" Int,
      HasSource "nextName" Int
    )
    via Field "nextName" () (MonadState (ExceptT Error (State (Env primTy primVal))))
  deriving
    ( HasState "nameStack" [Int],
      HasSink "nameStack" [Int],
      HasSource "nameStack" [Int]
    )
    via Field "nameStack" () (MonadState (ExceptT Error (State (Env primTy primVal))))
  deriving
    (HasThrow "erasureError" Error)
    via MonadError (ExceptT Error (MonadState (State (Env primTy primVal))))

data Error
  = Unsupported
  | CannotEraseZeroUsageTerm Text
  | InternalError Text
  deriving (Show, Eq, Generic)
