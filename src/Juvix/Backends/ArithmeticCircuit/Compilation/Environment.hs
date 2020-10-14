{-# LANGUAGE TupleSections #-}

module Juvix.Backends.ArithmeticCircuit.Compilation.Environment where

import qualified Juvix.Backends.ArithmeticCircuit.Compilation.Memory as Memory
import qualified Juvix.Backends.ArithmeticCircuit.Compilation.Types as Types
import Juvix.Library

type Memory = Memory.T Types.Expression

data Env
  = Env
      { memory :: Memory.T Types.Expression,
        compilation :: Types.Expression
      }
  deriving (Generic, Show)

type CompilationAlias = ExceptT Types.CompilationError (State Env)

newtype Compilation a = Compilation {antiAlias :: (CompilationAlias a)}
  deriving (Functor, Applicative, Monad)
  deriving
    ( HasState "memory" Memory,
      HasSink "memory" Memory,
      HasSource "memory" Memory
    )
    via StateField "memory" CompilationAlias
  deriving
    ( HasState "compilation" Types.Expression,
      HasSink "compilation" Types.Expression,
      HasSource "compilation" Types.Expression
    )
    via StateField "compilation" CompilationAlias
  deriving
    (HasThrow "compilationError" Types.CompilationError)
    via MonadError CompilationAlias

type HasMemory m = HasState "memory" Memory m

type HasComp m = (HasMemory m, HasState "compilation" Types.Expression m)

type HasMemoryErr m =
  (HasMemory m, HasThrow "compilationError" Types.CompilationError m)

type HasCompErr m =
  (HasComp m, HasThrow "compilationError" Types.CompilationError m)

insert, alloc :: HasMemory m => Symbol -> Types.Expression -> m Types.Expression
insert sy exp =
  modify @"memory" (Memory.alloc sy exp) *> return exp
alloc = insert

insertExternal, allocExternal :: HasMemory m => Symbol -> m ()
insertExternal sy =
  modify @"memory" (Memory.allocExternal sy Types.NoExp)
allocExternal = insertExternal

remove, free :: HasMemory m => Symbol -> m ()
remove sy =
  modify @"memory" (Memory.free sy)
--
free = remove

lookupErr :: HasMemoryErr m => Symbol -> m (Memory.Ele Types.Expression)
lookupErr sy = do
  mem <- get @"memory"
  case Memory.lookup sy mem of
    Just res -> pure res
    Nothing -> throw @"compilationError" Types.VariableOutOfScope

write :: HasState "compilation" s m => s -> m s
write exp = do
  put @"compilation" exp
  return exp

read :: HasState "compilation" s m => m s
read = get @"compilation"
