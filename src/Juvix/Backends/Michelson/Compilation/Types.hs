-- |
-- - Types used internally by the Michelson backend.
module Juvix.Backends.Michelson.Compilation.Types where

import Juvix.Backends.Michelson.Parameterisation
import Juvix.Library
import qualified Michelson.TypeCheck as M
import qualified Michelson.Typed as MT
import qualified Michelson.Untyped as M
import Prelude (error)

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
  deriving (Show, Eq)

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

ins ∷ (StackElem, M.Type) → (Int → Int) → Stack → Stack
ins v f (Stack stack' size) = Stack (v : stack') (f size)

-- | 'inStack' determines if the given
inStack ∷ StackElem → Bool
inStack (VarE _ (Just FuncResultE)) = True
inStack (VarE _ (Just (ConstE _))) = False
inStack (VarE _ Nothing) = True
inStack (Val (ConstE _)) = False
inStack (Val FuncResultE) = True

-- invariant ¬ inStack = valueOf is valid!
valueOfErr ∷ StackElem → Value
valueOfErr (VarE _ (Just (ConstE i))) = i
valueOfErr (Val (ConstE i)) = i
valueOfErr (Val FuncResultE) = error "called valueOf with a stored value"
valueOfErr (VarE _ Nothing) = error "called valueOf with a stored value"
valueOfErr (VarE _ (Just FuncResultE)) = error "called valueOf with a stored value"

-- | 'carErr' gets the first element off the stack
-- may return an error
car ∷ Stack → (StackElem, M.Type)
car (Stack (s : _) _) = s
car (Stack [] _) = error "Called car on an empty list"

-- | 'cdr' removes the first element of the list
cdr ∷ Stack → Stack
cdr (Stack (s : ss) size)
  | inStack (fst s) = Stack ss (pred size)
  | otherwise = Stack ss size
cdr (Stack [] size) = Stack [] size

-- | 'cons' is like 'consStack', however it works ont ehs tack directly,
-- not from within a monad
cons ∷ (StackElem, M.Type) → Stack → Stack
cons v = ins v f
  where
    f
      | inStack (fst v) = succ
      | otherwise = identity

-- | 'consStack', cons on a value v to our representation of the stack
-- This stack may have more values tha the real one, as we store
-- constants on this stack for resolution, however these will not appear
-- in the real michelson stack
consStack ∷ HasState "stack" Stack m ⇒ (StackElem, M.Type) → m ()
consStack = modify @"stack" . cons
