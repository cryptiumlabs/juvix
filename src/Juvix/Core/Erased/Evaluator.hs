module Juvix.Core.Erased.Evaluator
  ( evaluate,
  )
where

import Juvix.Core.Erased.Types
import Juvix.Library hiding (Map, evaluate)
import qualified Juvix.Library.HashMap as Map

evaluate ∷
  ∀ primVal m.
  ( HasReader "apply" (primVal → primVal → Maybe primVal) m,
    HasState "env" (Map.Map Symbol (Term primVal)) m
  ) ⇒
  Term primVal →
  m (Term primVal)
evaluate term =
  case term of
    Var s → do
      env ← get @"env"
      case Map.lookup s env of
        Just v → pure v
        Nothing → pure (Var s)
    Prim p → pure (Prim p)
    Lam s t → Lam s |<< evaluate t
    App f x → do
      f ← evaluate f
      x ← evaluate x
      case (f, x) of
        (Prim f', Prim x') → do
          apply ← ask @"apply"
          case apply f' x' of
            Just r → pure (Prim r)
            Nothing → undefined
        (Lam s t, v) → do
          modify @"env" (Map.insert s v)
          evaluate t
        _ → pure (App f x)
