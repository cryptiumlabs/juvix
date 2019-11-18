module Juvix.Interpreter.InteractionNet
  ( module Juvix.Interpreter.InteractionNet.Parser,
    module Juvix.Interpreter.InteractionNet.Translation,
    module Juvix.Interpreter.InteractionNet.Type,
    module Juvix.Interpreter.InteractionNet.Default,
    erasedCoreToInteractionNetAST,
  )
where

import qualified Juvix.Core.Erased.Types as Erased
import Juvix.Interpreter.InteractionNet.Default
import Juvix.Interpreter.InteractionNet.Parser
import Juvix.Interpreter.InteractionNet.Translation
import Juvix.Interpreter.InteractionNet.Type

erasedCoreToInteractionNetAST ∷ ∀ primVal. Erased.Term primVal → AST primVal
erasedCoreToInteractionNetAST term =
  case term of
    Erased.Var s → Symbol' s
    Erased.Prim p → Prim p
    Erased.Lam s t → Lambda s (erasedCoreToInteractionNetAST t)
    Erased.App f x → Application (erasedCoreToInteractionNetAST f) (erasedCoreToInteractionNetAST x)
