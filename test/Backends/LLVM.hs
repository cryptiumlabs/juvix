module Backends.LLVM where

import Juvix.Backends.LLVM.JIT
import Juvix.Library
import LLVM.AST
import LLVM.AST.AddrSpace
import qualified LLVM.AST.Attribute as A
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Global as G
import qualified LLVM.AST.Linkage as L
import LLVM.AST.Name
import LLVM.AST.Type
import qualified LLVM.AST.Visibility as V
import LLVM.Context
import LLVM.ExecutionEngine
import LLVM.Module
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

test_example_jit ∷ T.TestTree
test_example_jit = T.testCase "example module should jit function" $ do
  (fn :: Word32 -> IO Word32) ← jit (Config None) exampleModule "_foo"
  res ← fn 7
  42 T.@=? res

exampleModule ∷ LLVM.AST.Module
exampleModule =
  Module
    "runSomethingModule"
    "runSomethingModule"
    Nothing
    Nothing
    [ GlobalDefinition $
        functionDefaults
          { G.returnType = i32,
            G.name = Name "_foo",
            G.parameters = ([Parameter i32 (Name "bar") []], False),
            G.basicBlocks =
              [ BasicBlock
                  (UnName 0)
                  []
                  ( Do $ Ret (Just (ConstantOperand (C.Int 32 42))) []
                  )
              ]
          }
    ]
