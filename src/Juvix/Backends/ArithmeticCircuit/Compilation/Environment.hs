{-# LANGUAGE ConstraintKinds, TupleSections #-}

module Juvix.Backends.ArithmeticCircuit.Compilation.Environment where

import Juvix.Library
import Juvix.Backends.ArithmeticCircuit.Compilation.Types
import qualified Data.Map as Map
import qualified Control.Monad.Fail as Fail


data Memory = Mem (Map.Map Symbol (Int, ArithExpression)) Int
  deriving (Generic)

instance Semigroup Memory where
  (Mem map_ n) <> (Mem map__ m) = Mem (map_ <> Map.map (shift n) map__) (n + m)
    where shift n (m, exp) = (n + m, exp)

instance Monoid Memory where
  mempty = Mem Map.empty 0
  mappend = (<>)

data Env = Env { memory :: Memory
               , compilation :: ArithExpression
               }
  deriving (Generic)

type ArithmeticCircuitCompilationAlias = ExceptT CompilationError (State Env)

newtype ArithmeticCircuitCompilation a = Compilation { antiAlias :: (ArithmeticCircuitCompilationAlias a) }
  deriving (Functor, Applicative, Monad)
  deriving
    (HasState "memory" Memory
    , HasSink "memory" Memory
    , HasSource "memory" Memory
    ) via StateField "memory" ArithmeticCircuitCompilationAlias
  deriving
    (HasState "compilation" ArithExpression
    , HasSink "compilation" ArithExpression
    , HasSource "compilation" ArithExpression
    ) via StateField "compilation" ArithmeticCircuitCompilationAlias
  deriving
    (HasThrow "compilationError" CompilationError)
    via MonadError ArithmeticCircuitCompilationAlias

insert :: Symbol -> ArithExpression -> ArithmeticCircuitCompilation ArithExpression
insert sy exp =
  do
    modify @"memory" (insert' sy exp)
    return exp
  where
    insert' :: Symbol -> ArithExpression -> Memory -> Memory
    insert' sy x (Mem map_ n) = Mem (Map.insert sy (n, x) map_) (n + 1)

remove :: Symbol -> ArithmeticCircuitCompilation ()
remove sy =
  do
    modify @"memory" (remove' sy)
  where
    remove' :: Symbol -> Memory -> Memory
    remove' sy (Mem map_ n) = Mem (Map.delete sy map_) (n - 1)

lookup :: Symbol -> ArithmeticCircuitCompilation (Int, ArithExpression)
lookup sy =
  do
    mem <- get @"memory"
    lookup' sy mem
  where
    lookup' :: Symbol -> Memory -> ArithmeticCircuitCompilation (Int, ArithExpression)
    lookup' sy (Mem map_ _) =
      case Map.lookup sy map_ of
        Just res -> return res
        Nothing -> throw @"compilationError" VariableOutOfScope

write :: HasState "compilation" s m => s -> m s
write exp =
  do put @"compilation" exp
     return exp

read :: HasState "compilation" s m => m s
read = get @"compilation"

freshVars :: [Symbol] -> ArithmeticCircuitCompilation ()
freshVars sys = modify @"memory" (freshVars' sys)
  where
    freshVars' :: [Symbol] -> Memory -> Memory
    freshVars' vars (Mem map_ n) = Mem (map_ <> Map.fromList (zip vars (slots n))) (n + 1)

    slots :: Int -> [(Int, ArithExpression)]
    slots n = map (, NoExp) [n ..]
