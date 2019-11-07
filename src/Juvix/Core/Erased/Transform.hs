module Juvix.Core.Erased.Transform where

import qualified Juvix.Bohm.Type as BT
import Juvix.Core.Erased.Types
import Juvix.Library

erasedCoreToBohm ∷ ∀ primVal. Term primVal → BT.Bohm
erasedCoreToBohm term =
  case term of
    Var s → BT.Symbol' s
    Prim _ → undefined
    Lam s t → BT.Lambda s (erasedCoreToBohm t)
    App f x → BT.Application (erasedCoreToBohm f) (erasedCoreToBohm x)
