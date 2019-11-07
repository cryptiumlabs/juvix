module Juvix.Bohm
  ( module Juvix.Bohm.Parser,
    module Juvix.Bohm.Translation,
    module Juvix.Bohm.Type,
    module Juvix.Bohm.Default,
    erasedCoreToBohm,
  )
where

import Juvix.Bohm.Default
import Juvix.Bohm.Parser
import Juvix.Bohm.Translation
import Juvix.Bohm.Type
import qualified Juvix.Core.Erased.Types as Erased
import Juvix.Library

erasedCoreToBohm ∷ ∀ primVal. Erased.Term primVal → Bohm
erasedCoreToBohm term =
  case term of
    Erased.Var s → Symbol' s
    Erased.Prim _ → undefined
    Erased.Lam s t → Lambda s (erasedCoreToBohm t)
    Erased.App f x → Application (erasedCoreToBohm f) (erasedCoreToBohm x)
