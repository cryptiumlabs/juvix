-- |
-- Translates erased core terms (must be EAC-typable) to native interaction nets in LLVM, evaluates it, and reads-back the resulting term.
-- TODO: Separate out the common logic from the interpreter & this file into a shared module.
module Juvix.Backends.LLVM.Translation where

import qualified Data.HashMap.Strict as Map
import qualified Juvix.Backends.LLVM.Codegen as Codegen
import Juvix.Backends.LLVM.JIT
import Juvix.Backends.LLVM.Net.Environment
import qualified Juvix.Core.Erased.Types as Erased
import qualified Juvix.Core.Types as Core
import Juvix.Interpreter.InteractionNet hiding (Erase, Lambda)
import qualified Juvix.Interpreter.InteractionNet.Backends.Graph as Graph
import Juvix.Interpreter.InteractionNet.Backends.Interface
import Juvix.Interpreter.InteractionNet.Nets.Default
import Juvix.Library hiding (empty, reduce)

evalErasedCoreInLLVM ∷
  ∀ primTy primVal m.
  (MonadIO m) ⇒
  Core.Parameterisation primTy primVal →
  Erased.Term primVal →
  m (Erased.Term primVal)
evalErasedCoreInLLVM parameterisation term = do
  -- Generate the LLVM module.
  let mod = Codegen.moduleAST runInitModule
  -- JIT the module.
  (NetAPI createNet appendToNet readNet reduceUntilComplete, kill) ← liftIO (jitToNetAPI (Config None) mod)
  -- Convert the term to a graph.
  let netAST = erasedCoreToInteractionNetAST term

      graph ∷ Graph.FlipNet (Lang primVal)
      graph = astToNet parameterisation netAST Map.empty

  -- Walk the graph; fetch all nodes.
  let ns = flip evalEnvState (Env 0 graph Map.empty) $ do
        nodes ← nodes
        ann ← flip mapM nodes $ \n → do
          lang ← langToPort n (\l → pure (pure l))
          let Just l = lang
          edges ← allEdges n
          pure (n, l, edges)
        pure ann
  -- Create a new net.
  net ← liftIO createNet
  -- Append the nodes.
  let nodes = [] -- TODO
  liftIO (appendToNet net nodes)
  -- Reduce it.
  liftIO (reduceUntilComplete net)
  -- Read-back the nodes
  nodes ← liftIO (readNet net)
  let graph ∷ Graph.FlipNet (Lang primVal)
      graph = undefined
  -- TODO: read-back term
  let res ∷ Erased.Term primVal
      Just res = interactionNetASTToErasedCore |<< netToAst graph
  -- Free the module.
  liftIO kill
  pure res
