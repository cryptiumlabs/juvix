{-# LANGUAGE TupleSections #-}

module Juvix.Backends.ArithmeticCircuit.Compilation.Environment where

import qualified Data.Map as Map
import qualified Juvix.Backends.ArithmeticCircuit.Compilation.Types as Types
import Juvix.Library

data Memory = Mem (Map.Map Symbol (Int, Types.Expression)) Int
  deriving (Generic)

instance Semigroup Memory where
  (Mem map' n) <> (Mem map'' m) = Mem (map' <> fmap (shift n) map'') (n + m)
    where
      shift n (m, exp) = (n + m, exp)

instance Monoid Memory where
  mempty = Mem Map.empty 0
  mappend = (<>)

data Env
  = Env
      { memory :: Memory,
        compilation :: Types.Expression
      }
  deriving (Generic)

type CompilationAlias = ExceptT Types.CompilationError (State Env)

newtype Compilation a
  = Compilation {antiAlias :: (CompilationAlias a)}
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

insert :: HasMemory m => Symbol -> Types.Expression -> m Types.Expression
insert sy exp =
  modify @"memory" (insert' sy exp) *> return exp
  where
    insert' :: Symbol -> Types.Expression -> Memory -> Memory
    insert' sy x (Mem map n) = Mem (Map.insert sy (n, x) map) (succ n)

remove :: HasMemory m => Symbol -> m ()
remove sy =
  modify @"memory" (remove' sy)
  where
    remove' :: Symbol -> Memory -> Memory
    remove' sy (Mem map n) = Mem (Map.delete sy map) (pred n)

lookup :: HasMemoryErr m => Symbol -> m (Int, Types.Expression)
lookup sy = do
  Mem map _ <- get @"memory"
  case Map.lookup sy map of
    Just res -> return res
    Nothing -> throw @"compilationError" Types.VariableOutOfScope

write :: HasState "compilation" s m => s -> m s
write exp = do
  put @"compilation" exp
  return exp

read :: HasState "compilation" s m => m s
read = get @"compilation"

freshVars :: HasMemory m => [Symbol] -> m ()
freshVars sys = modify @"memory" (freshVars' sys)
  where
    freshVars' :: [Symbol] -> Memory -> Memory
    freshVars' vars (Mem map' n) =
      Mem (map' <> Map.fromList (zip vars (slots n))) (succ n)
    --
    slots :: Int -> [(Int, Types.Expression)]
    slots n = fmap (,Types.NoExp) [n ..]
