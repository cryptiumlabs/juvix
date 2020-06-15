{-# LANGUAGE ConstraintKinds #-}

module Juvix.Backends.ArithmeticCircuit.Compilation.Environment where

import Juvix.Library
import Juvix.Backends.ArithmeticCircuit.Compilation.Types
import qualified Data.Map as Map
import qualified Juvix.Backends.ArithmeticCircuit.Parameterisation as Par

data Memory = Mem (Map.Map Symbol (Int, ArithExpression Par.F)) Int

instance Semigroup Memory where
  (Mem map_ n) <> (Mem map__ m) = Mem (map_ <> Map.map (shift n) map__) (n + m)
    where shift n (m, exp) = (n + m, exp)

instance Monoid Memory where
  mempty = Mem Map.empty 0
  mappend = (<>)

data Env = Env { memory :: Memory
               , compilation :: ArithExpression Par.F
               }

insert :: Symbol -> ArithExpression Par.F -> Memory -> Memory
insert sy x (Mem map_ n) = Mem (Map.insert sy (n, x) map_) (n + 1)

lookup :: Symbol -> Memory -> Maybe (Int, ArithExpression Par.F)
lookup sy (Mem map_ _) =
  case Map.lookup sy map_ of
    Just res -> Just res
    Nothing -> Nothing

type ArithmeticCircuitCompilationAlias = ExceptT CompilationError (State Env)

newtype ArithmeticCircuitCompilation a = Compilation (ArithmeticCircuitCompilationAlias a)
  deriving (Functor, Applicative, Monad)
  deriving
    (HasState "memory" Memory
    , HasSink "memory" Memory
    , HasSource "memory" Memory
    ) via StateField "memory" ArithmeticCircuitCompilationAlias
  deriving
    (HasState "compilation" ArithExpression Par.F
    , HasSink "memory" Memory
    , HasSource "memory" Memory
    ) via StateField "compilation" ArithmeticCircuitCompilationAlias
