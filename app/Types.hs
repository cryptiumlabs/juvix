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

type EnvExecAlias primTy primVal =
  ExceptT (Core.PipelineError primTy primVal)
    (StateT (Env primTy primVal) IO)

newtype EnvExec primTy primVal a
  = EnvE (EnvExecAlias primTy primVal a)
  deriving (Functor, Applicative, Monad, MonadIO)
  deriving
    (HasSink "log" [Core.PipelineLog primTy primVal],
     HasWriter "log" [Core.PipelineLog primTy primVal])
  via WriterField "log" (EnvExecAlias primTy primVal)
  deriving
    (HasReader "parameterisation" (Core.Parameterisation primTy primVal),
     HasSource "parameterisation" (Core.Parameterisation primTy primVal))
  via ReaderField "parameterisation" (EnvExecAlias primTy primVal)
  deriving (HasThrow "error" (Core.PipelineError primTy primVal))
  via MonadError (EnvExecAlias primTy primVal)

data SomeBackend where
  SomeBackend :: forall primTy primVal. Core.Parameterisation primTy primVal -> SomeBackend
