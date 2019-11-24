module Backends.LLVM where

import Juvix.Backends.LLVM.Codegen
import Juvix.Backends.LLVM.JIT
import Juvix.Library
import LLVM.AST
import LLVM.AST.AddrSpace
import qualified LLVM.AST.Attribute as A
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Global as G
import qualified LLVM.AST.Instruction as I (function)
import qualified LLVM.AST.Linkage as L
import LLVM.AST.Name
import LLVM.AST.Type
import qualified LLVM.AST.Visibility as V
import LLVM.Context
import LLVM.ExecutionEngine
import LLVM.Module
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

test_malloc_free_jit ∷ T.TestTree
test_malloc_free_jit = T.testCase "malloc free module should jit" $ do
  (fn ∷ Word32 → IO Word32) ← jit (Config None) mallocFreeModule "test"
  res ← fn 30
  30 T.@=? res

mallocFreeModule ∷ LLVM.AST.Module
mallocFreeModule =
  Module
    "mallocFreeModule"
    "mallocFreeModule"
    Nothing
    Nothing
    [ GlobalDefinition $
        functionDefaults
          { G.returnType = voidStarTy,
            G.name = Name "malloc",
            G.parameters = ([Parameter (IntegerType {typeBits = 64}) (Name "size") []], False),
            G.callingConvention = CC.Fast,
            G.basicBlocks = [],
            G.linkage = L.External
          },
      GlobalDefinition $
        functionDefaults
          { G.returnType = voidTy,
            G.name = Name "free",
            G.parameters = ([Parameter voidStarTy (Name "") []], False),
            G.callingConvention = CC.Fast,
            G.basicBlocks = [],
            G.linkage = L.External
          },
      GlobalDefinition $
        functionDefaults
          { G.returnType = i32,
            G.name = Name "test",
            G.parameters = ([Parameter i32 (Name "bar") []], False),
            G.basicBlocks =
              [ BasicBlock
                  (UnName 0)
                  [ UnName 1 := Call
                      { tailCallKind = Nothing,
                        I.function =
                          Right
                            ( ConstantOperand
                                ( C.GlobalReference
                                    (ptr $ FunctionType {resultType = voidStarTy, argumentTypes = [IntegerType {typeBits = 64}], isVarArg = False})
                                    (Name "malloc")
                                )
                            ),
                        callingConvention = CC.C,
                        returnAttributes = [],
                        arguments = [(LocalReference i32 (Name "bar"), [])],
                        functionAttributes = [],
                        metadata = []
                      },
                    Do $ Call
                      { tailCallKind = Nothing,
                        I.function =
                          Right
                            ( ConstantOperand
                                ( C.GlobalReference
                                    (ptr $ FunctionType {resultType = voidTy, argumentTypes = [voidStarTy], isVarArg = False})
                                    (Name "free")
                                )
                            ),
                        callingConvention = CC.C,
                        returnAttributes = [],
                        arguments = [(LocalReference voidStarTy (UnName 1), [])],
                        functionAttributes = [],
                        metadata = []
                      }
                  ]
                  ( Do $ Ret (Just (LocalReference i32 (Name "bar"))) []
                  )
              ]
          }
    ]

test_example_jit ∷ T.TestTree
test_example_jit = T.testCase "example module should jit function" $ do
  (fn ∷ Word32 → IO Word32) ← jit (Config None) exampleModule "_foo"
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
