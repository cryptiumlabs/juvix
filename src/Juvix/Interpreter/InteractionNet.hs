module Juvix.Interpreter.InteractionNet
  ( module Juvix.Interpreter.InteractionNet.Parser,
    module Juvix.Interpreter.InteractionNet.Translation,
    module Juvix.Interpreter.InteractionNet.Type,
    module Juvix.Interpreter.InteractionNet.Default,
    erasedCoreToInteractionNetAST,
    interactionNetASTToErasedCore,
  )
where

import qualified Juvix.Core.Erased.Types as Erased
import Juvix.Interpreter.InteractionNet.Default
import Juvix.Interpreter.InteractionNet.Parser
import Juvix.Interpreter.InteractionNet.Translation
import Juvix.Interpreter.InteractionNet.Type

erasedCoreToInteractionNetAST :: forall primVal. Erased.Term primVal -> AST primVal
erasedCoreToInteractionNetAST term =
  case term of
    Erased.Var s -> Symbol' s
    Erased.Prim p -> Prim p
    Erased.Lam s t -> Lambda s (erasedCoreToInteractionNetAST t)
    Erased.App f x ->
      Application (erasedCoreToInteractionNetAST f) (erasedCoreToInteractionNetAST x)

interactionNetASTToErasedCore :: forall primVal. AST primVal -> Erased.Term primVal
interactionNetASTToErasedCore ast =
  case ast of
    Symbol' s -> Erased.Var s
    Prim p -> Erased.Prim p
    Lambda s t ->
      Erased.Lam s (interactionNetASTToErasedCore t)
    Application f x ->
      Erased.App (interactionNetASTToErasedCore f) (interactionNetASTToErasedCore x)
