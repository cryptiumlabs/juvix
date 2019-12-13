-- |
-- Translates erased core terms (must be EAC-typable) to native interaction nets in LLVM.
module Juvix.Backends.LLVM.Translation where

import qualified Data.HashMap.Strict as Map
import qualified Juvix.Backends.LLVM.Codegen as Codegen
import qualified Juvix.Backends.LLVM.Net.EAC as EAC
import qualified Juvix.Core.Erased.Types as Erased
import qualified Juvix.Core.Types as Core
import Juvix.Interpreter.InteractionNet hiding (Erase, Lambda)
import qualified Juvix.Interpreter.InteractionNet.Backends.Graph as Graph
import Juvix.Interpreter.InteractionNet.Backends.Interface
import Juvix.Interpreter.InteractionNet.Nets.Default
import Juvix.Library hiding (empty, reduce)
import qualified LLVM.AST as AST
import qualified LLVM.AST.Name as Name
import qualified LLVM.AST.Operand as Operand

{-
 - TODO: Separate out the common logic from the interpreter & this file into a shared module.
 -}

erasedCoreToLLVM ∷
  ∀ primTy primVal m.
  ( HasThrow "err" Codegen.Errors m,
    HasState "blockCount" Int m,
    HasState "blocks" (Map.HashMap Name.Name Codegen.BlockState) m,
    HasState "count" Word m,
    HasState "currentBlock" Name.Name m,
    HasState "moduleDefinitions" [AST.Definition] m,
    HasState "names" Codegen.Names m,
    HasState "typTab" Codegen.TypeTable m,
    HasState "varTab" Codegen.VariantToType m,
    HasState "symTab" Codegen.SymbolTable m
  ) ⇒
  Core.Parameterisation primTy primVal →
  Erased.Term primVal →
  m ()
erasedCoreToLLVM parameterisation term = do
  let netAST = erasedCoreToInteractionNetAST term

      graph ∷ Graph.FlipNet (Lang primVal)
      graph = astToNet parameterisation netAST Map.empty

  networkToLLVM graph

networkToLLVM ∷
  ∀ primVal m.
  ( HasThrow "err" Codegen.Errors m,
    HasState "blockCount" Int m,
    HasState "blocks" (Map.HashMap Name.Name Codegen.BlockState) m,
    HasState "count" Word m,
    HasState "currentBlock" Name.Name m,
    HasState "moduleDefinitions" [AST.Definition] m,
    HasState "names" Codegen.Names m,
    HasState "typTab" Codegen.TypeTable m,
    HasState "varTab" Codegen.VariantToType m,
    HasState "symTab" Codegen.SymbolTable m
  ) ⇒
  Graph.FlipNet (Lang primVal) →
  m ()
networkToLLVM n = do
  let ns = flip evalEnvState (Env 0 n Map.empty) $ do
        nodes ← nodes
        ann ← flip mapM nodes $ \n → do
          lang ← langToPort n (\l → pure (pure l))
          let Just l = lang
          edges ← allEdges n
          pure (n, l, edges)
        pure ann
  pure ()
