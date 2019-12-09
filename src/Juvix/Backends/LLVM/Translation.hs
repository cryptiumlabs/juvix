-- |
-- Translates erased core terms (must be EAC-typable) to native interaction nets in LLVM.
module Juvix.Backends.LLVM.Translation where

import Juvix.Library hiding (empty)
import qualified Juvix.Interpreter.InteractionNet.Backends.Graph as Graph
import qualified Data.HashMap.Strict as Map
import Juvix.Interpreter.InteractionNet
import Juvix.Interpreter.InteractionNet.Translation
import Juvix.Interpreter.InteractionNet.Nets.Default
import Juvix.Interpreter.InteractionNet.Backends.Interface
import qualified Juvix.Core.Erased.Types as Erased
import Juvix.Backends.LLVM.Codegen
import qualified Juvix.Core.Types as Core

{-
 - TODO: Separate out the common logic from the interpreter & this file into a shared module.
 -}

erasedCoreToLLVM :: forall primTy primVal . Core.Parameterisation primTy primVal -> Erased.Term primVal -> LLVM ()
erasedCoreToLLVM parameterisation term = do
  let netAST = erasedCoreToInteractionNetAST term
      graph :: Graph.FlipNet (Lang primVal)
      graph = astToNet parameterisation netAST Map.empty
  networkToLLVM graph

networkToLLVM :: forall primVal . Graph.FlipNet (Lang primVal) -> LLVM ()
networkToLLVM n = do
  let ns =  flip evalEnvState (Env 0 n Map.empty) $ do
              nodes <- nodes
              ann <- flip mapM nodes $ \n -> do
                lang <- langToPort n (\l -> pure (pure l))
                edges <- allEdges n
                pure (n, lang, edges)
              pure ann
  undefined
