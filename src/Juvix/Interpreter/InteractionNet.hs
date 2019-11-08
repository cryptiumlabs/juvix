module Juvix.Interpreter.InteractionNet
  ( module Juvix.Interpreter.InteractionNet.Parser,
    module Juvix.Interpreter.InteractionNet.Translation,
    module Juvix.Interpreter.InteractionNet.Type,
    module Juvix.Interpreter.InteractionNet.Default,
    erasedCoreToInteractionNetAST
  )
where

import Juvix.Interpreter.InteractionNet.Default
import Juvix.Interpreter.InteractionNet.Parser
import Juvix.Interpreter.InteractionNet.Translation
import Juvix.Interpreter.InteractionNet.Type
import qualified Juvix.Core.Erased.Types as Erased
import Juvix.Library

erasedCoreToInteractionNetAST ∷ ∀ primVal. Erased.Term primVal → Bohm
erasedCoreToInteractionNetAST term =
  case term of
    Erased.Var s → Symbol' s
    Erased.Prim _ → undefined
    Erased.Lam s t → Lambda s (erasedCoreToInteractionNetAST t)
    Erased.App f x → Application (erasedCoreToInteractionNetAST f) (erasedCoreToInteractionNetAST x)
