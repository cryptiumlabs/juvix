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
import qualified Juvix.INetIR.Types as INIR
import Juvix.Interpreter.InteractionNet hiding (Erase, Lambda, Prim)
import qualified Juvix.Interpreter.InteractionNet.Backends.Graph as Graph
import Juvix.Interpreter.InteractionNet.Backends.Interface
import Juvix.Interpreter.InteractionNet.Nets.Default
import Juvix.Library hiding (empty, link, reduce)
import Prelude ((!!))
import LLVM.Pretty

evalErasedCoreInLLVM ∷
  ∀ primTy primVal m.
  (MonadIO m) ⇒
  Core.Parameterisation primTy primVal →
  Erased.Term primVal →
  m (Erased.Term primVal)
evalErasedCoreInLLVM parameterisation term = do
  -- Generate the LLVM module.
  let mod = Codegen.moduleAST runInitModule
  -- Pretty-print the module.
  putStr (ppllvm mod) >> putStr ("\n" :: Text)
  -- JIT the module.
  liftIO (putText "Just-in-time compiling initial module...")
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
  liftIO (putText "Creating net...")
  net ← liftIO createNet
  -- Append the nodes.
  let nodes = flip map (zip [0 ..] ns) $ \(ind, (_, l, edges)) →
        INIR.Node
          { INIR.nodeAddress = ind,
            INIR.nodeKind = case l of
              Primar Erase → INIR.Eraser
              Auxiliary2 Lambda → INIR.Constructor 0
              Auxiliary2 App → INIR.Constructor 1
              Auxiliary2 (FanIn i) → INIR.Constructor (fromIntegral i),
            INIR.nodePorts = map (\(_, toNode, toPort) → INIR.Port toNode (portTypeToIndex toPort)) edges
          }
  liftIO (appendToNet net nodes)
  -- Reduce it.
  liftIO (reduceUntilComplete net)
  -- Read-back the nodes
  nodes ← liftIO (readNet net)
  -- Translate into a native graph.
  let graph ∷ Graph.FlipNet (Lang primVal)
      graph = flip evalEnvState (Env 0 empty Map.empty) $ do
        ns ← flip mapM nodes $ \node →
          newNode $ case INIR.nodeKind node of
            INIR.Eraser → Primar Erase
            INIR.Constructor 0 → Auxiliary2 Lambda
            INIR.Constructor 1 → Auxiliary2 App
            INIR.Constructor n → Auxiliary2 (FanIn (fromIntegral n))
        flip mapM_ nodes $ \node → do
          let addr = ns !! INIR.nodeAddress node
          flip mapM_ (zip [0 ..] $ INIR.nodePorts node) $ \(slot, (INIR.Port otherAddr' otherSlot)) → do
            let otherAddr = ns !! otherAddr'
            -- TODO: Double-linkage?
            link (addr, indexToPortType slot) (otherAddr, indexToPortType otherSlot)
        net ← get @"net"
        pure net
  -- Read-back the graph.
  let res ∷ Erased.Term primVal
      Just res = interactionNetASTToErasedCore |<< netToAst graph
  -- Free the module.
  liftIO kill
  -- Return the resulting term.
  pure res

portTypeToIndex ∷ PortType → INIR.Slot
portTypeToIndex Prim = 0
portTypeToIndex Aux1 = 1
portTypeToIndex Aux2 = 2
portTypeToIndex Aux3 = 3
portTypeToIndex Aux4 = 4
portTypeToIndex Aux5 = 5

indexToPortType ∷ INIR.Slot → PortType
indexToPortType 0 = Prim
indexToPortType 1 = Aux1
indexToPortType 2 = Aux2
indexToPortType 3 = Aux3
indexToPortType 4 = Aux4
indexToPortType 5 = Aux5
