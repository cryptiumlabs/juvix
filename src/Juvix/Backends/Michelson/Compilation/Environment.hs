module Juvix.Backends.Michelson.Compilation.Environment
  ( module Juvix.Backends.Michelson.Compilation.Types,
    module Juvix.Backends.Michelson.Compilation.Environment,
    VStack.car,
    VStack.cdr,
    VStack.cons,
    VStack.LamPartial (..),
  )
where

import Juvix.Backends.Michelson.Compilation.Types
import qualified Juvix.Backends.Michelson.Compilation.VirtualStack as VStack
import Juvix.Library

type VStack = VStack.T

data Env
  = Env
      { stack ∷ VStack.T,
        compilationLog ∷ [CompilationLog]
      }
  deriving (Generic)

newtype EnvCompilation a = EnvCompilation (ExceptT CompilationError (State Env) a)
  deriving (Functor, Applicative, Monad)
  deriving
    ( HasStream "compilationLog" [CompilationLog],
      HasWriter "compilationLog" [CompilationLog]
    )
    via WriterLog (Field "compilationLog" () (MonadState (ExceptT CompilationError (State Env))))
  deriving
    (HasState "stack" VStack.T)
    via Field "stack" () (MonadState (ExceptT CompilationError (State Env)))
  deriving
    (HasThrow "compilationError" CompilationError)
    via MonadError (ExceptT CompilationError (State Env))

execWithStack ∷ VStack.T → EnvCompilation a → (Either CompilationError a, Env)
execWithStack stack (EnvCompilation env) = runState (runExceptT env) (Env stack [])
