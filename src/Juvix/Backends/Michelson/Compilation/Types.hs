-- |
-- - Types used internally by the Michelson backend.
module Juvix.Backends.Michelson.Compilation.Types where

import Juvix.Library
import qualified Michelson.TypeCheck as M
import qualified Michelson.Typed as MT
import qualified Michelson.Untyped as M
import qualified Juvix.Core.ErasedAnn.Types as CoreErased

data PrimTy
  = PrimTy M.Type
  deriving (Show, Eq, Generic)

data PrimVal
  = PrimConst (M.Value' Op)
  | PrimPair
  | PrimFst
  | PrimSnd
  -- TODO: Add all Michelson instructions which are functions.
  deriving (Show, Eq, Generic)

type Term = CoreErased.AnnTerm PrimTy PrimVal

type Type = CoreErased.Type PrimTy PrimVal

type Value = M.Value' M.ExpandedOp

type Op = M.ExpandedOp

data CompilationError
  = NotYetImplemented Text
  | InvalidInputType
  | InternalFault Text
  | DidNotTypecheck M.TCError
  | DidNotTypecheckAfterOptimisation M.TCError
  deriving (Show, Eq, Generic)

data CompilationLog
  = TermToInstr Term Op
  | OptimisedByJuvix Op Op
  | OptimisedByMorley SomeInstr SomeInstr
  deriving (Generic)

type Stack = [(StackElem, M.Type)]

data StackElem
  = ConstE Value
  | VarE Symbol
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
