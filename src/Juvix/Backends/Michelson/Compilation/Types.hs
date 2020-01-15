-- |
-- - Types used internally by the Michelson backend.
module Juvix.Backends.Michelson.Compilation.Types where

import Juvix.Backends.Michelson.Parameterisation
import Juvix.Library
import qualified Michelson.TypeCheck as M
import qualified Michelson.Typed as MT
import qualified Michelson.Untyped as M

data CompilationError
  = NotYetImplemented Text
  | InvalidInputType
  | InternalFault Text
  | DidNotTypecheck M.TCError
  | DidNotTypecheckAfterOptimisation M.TCError
  | -- Should never happen!
    NotEnoughStackSpace
  deriving (Show, Eq, Generic)

data CompilationLog
  = TermToInstr Term Op
  | OptimisedByJuvix Op Op
  | OptimisedByMorley SomeInstr SomeInstr
  deriving (Generic)

data Stack
  = Stack
      { stack' ∷ [(StackElem, M.Type)],
        size ∷ Int
      }

data StackElem
  = VarE Symbol (Maybe StackVal)
  | Val StackVal
  deriving (Show, Eq, Generic)

data StackVal
  = ConstE Value
  | FuncResultE
  deriving (Show, Eq, Generic)

data SomeInstr where
  SomeInstr ∷ ∀ a b. MT.Instr a b → SomeInstr

deriving instance Show (SomeInstr)

instance Eq SomeInstr where
  _ == _ = False

data Env
  = Env
      { stack ∷ Stack,
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
    (HasState "stack" Stack)
    via Field "stack" () (MonadState (ExceptT CompilationError (State Env)))
  deriving
    (HasThrow "compilationError" CompilationError)
    via MonadError (ExceptT CompilationError (State Env))

execWithStack ∷ Stack → EnvCompilation a → (Either CompilationError a, Env)
execWithStack stack (EnvCompilation env) = runState (runExceptT env) (Env stack [])

ins ∷
  HasState "stack" Stack m ⇒
  (StackElem, M.Type) →
  (Int → Int) →
  m ()
ins v f = do
  Stack stack' size ← get @"stack"
  put @"stack" (Stack (v : stack') (f size))

-- | 'consStack', cons on a value v to our representation of the stack
-- This stack may have more values tha the real one, as we store
-- constants on this stack for resolution, however these will not appear
-- in the real michelson stack
consStack ∷
  HasState "stack" Stack m ⇒
  (StackElem, M.Type) →
  m ()
consStack v@(VarE _ (Just FuncResultE), _) = ins v succ
consStack v@(VarE _ (Just (ConstE _)), _) = ins v identity
consStack v@(VarE _ Nothing, _) = ins v succ
consStack v@(Val (ConstE _), _) = ins v identity
consStack v@(Val FuncResultE, _) = ins v succ
