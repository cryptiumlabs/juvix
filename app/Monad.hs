module Monad where

import Juvix.Core.Parameterisations.Naturals
import qualified Juvix.Core.Types as Core
import Juvix.Library hiding (log)

exec ∷ EnvExec NatTy NatVal a → IO (Either (Core.PipelineError NatTy NatVal) a, [Core.PipelineLog NatTy NatVal])
exec (EnvE env) = do
  (ret, env) ← runStateT (runExceptT env) (Env nat [])
  pure (ret, log env)

data Env primTy primVal
  = Env
      { parameterisation ∷ Core.Parameterisation primTy primVal,
        log ∷ [Core.PipelineLog primTy primVal]
      }
  deriving (Generic)

newtype EnvExec primTy primVal a = EnvE (ExceptT (Core.PipelineError primTy primVal) (StateT (Env primTy primVal) IO) a)
  deriving (Functor, Applicative, Monad, MonadIO)
  deriving
    ( HasStream "log" [Core.PipelineLog primTy primVal],
      HasWriter "log" [Core.PipelineLog primTy primVal]
    )
    via WriterLog (Field "log" () (MonadState (ExceptT (Core.PipelineError primTy primVal) (StateT (Env primTy primVal) IO))))
  deriving
    -- TODO: Should be HasReader, this library is finicky.
    (HasState "parameterisation" (Core.Parameterisation primTy primVal))
    via (Field "parameterisation" () (MonadState (ExceptT (Core.PipelineError primTy primVal) (StateT (Env primTy primVal) IO))))
  deriving
    (HasThrow "error" (Core.PipelineError primTy primVal))
    via MonadError (ExceptT (Core.PipelineError primTy primVal) (StateT (Env primTy primVal) IO))
