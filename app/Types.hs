module Types where

import qualified Juvix.Core.Types as Core
import Juvix.Library hiding (log)

exec ::
  EnvExec primTy primVal a ->
  Core.Parameterisation primTy primVal ->
  IO (Either (Core.PipelineError primTy primVal) a, [Core.PipelineLog primTy primVal])
exec (EnvE env) param = do
  (ret, env) <- runStateT (runExceptT env) (Env param [])
  pure (ret, log env)

data Env primTy primVal
  = Env
      { parameterisation :: Core.Parameterisation primTy primVal,
        log :: [Core.PipelineLog primTy primVal]
      }
  deriving (Generic)

newtype EnvExec primTy primVal a
  = EnvE (ExceptT (Core.PipelineError primTy primVal) (StateT (Env primTy primVal) IO) a)
  deriving (Functor, Applicative, Monad, MonadIO)
  deriving
    ( HasStream "log" [Core.PipelineLog primTy primVal],
      HasWriter "log" [Core.PipelineLog primTy primVal]
    )
    via WriterLog
          ( Field "log" ()
              ( MonadState
                  ( ExceptT (Core.PipelineError primTy primVal)
                      (StateT (Env primTy primVal) IO)
                  )
              )
          )
  deriving
    (HasReader "parameterisation" (Core.Parameterisation primTy primVal))
    via Field "parameterisation" ()
          ( ReadStatePure
              ( MonadState
                  ( ExceptT (Core.PipelineError primTy primVal)
                      (StateT (Env primTy primVal) IO)
                  )
              )
          )
  deriving
    (HasThrow "error" (Core.PipelineError primTy primVal))
    via MonadError
          ( ExceptT (Core.PipelineError primTy primVal)
              (StateT (Env primTy primVal) IO)
          )

data SomeBackend where
  SomeBackend :: forall primTy primVal. Core.Parameterisation primTy primVal -> SomeBackend
