module Juvix.Core.Erased.Evaluator
  ( evaluate,
  )
where

import qualified Juvix.Core.Erased.Types as Erased
import Juvix.Library hiding (Map, evaluate)
import qualified Juvix.Library.HashMap as Map

evaluate ::
  forall primVal m.
  ( HasReader "apply" (primVal -> primVal -> Maybe primVal) m,
    HasReader "env" (Map.T Symbol (Erased.Term primVal)) m,
    HasThrow "evaluationError" (Erased.EvaluationError primVal) m
  ) =>
  Erased.Term primVal ->
  m (Erased.Term primVal)
evaluate term =
  case term of
    Erased.Var s -> do
      env <- ask @"env"
      case Map.lookup s env of
        Just v -> pure v
        Nothing -> pure (Erased.Var s)
    Erased.Prim p -> pure (Erased.Prim p)
    Erased.Lam s t -> Erased.Lam s |<< evaluate t
    Erased.Pair s t -> Erased.Pair <$> evaluate s <*> evaluate t
    Erased.Let s b t -> do
      b <- evaluate b
      local @"env" (Map.insert s b) $ evaluate t
    Erased.App f x -> do
      f <- evaluate f
      x <- evaluate x
      case (f, x) of
        (Erased.Prim f', Erased.Prim x') -> do
          apply <- ask @"apply"
          case apply f' x' of
            Just r -> pure (Erased.Prim r)
            Nothing ->
              throw @"evaluationError" (Erased.PrimitiveApplicationError f' x')
        (Erased.Lam s t, v) -> do
          local @"env" (Map.insert s v) $ evaluate t
        _ -> pure (Erased.App f x)
