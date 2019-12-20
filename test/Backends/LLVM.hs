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

backendLLVM ∷ T.TestTree
backendLLVM =
  T.testGroup
    "Backend LLVM"
    [ fn_test_malloc_free_jit,
      fn_test_example_jit
    ]

fn_test_malloc_free_jit ∷ T.TestTree
fn_test_malloc_free_jit = T.testCase "malloc free module should jit" $ do
  (imp, kill) ← jitWith (Config None) mallocFreeModule dynamicImport
  Just fn ← importAs imp "test" (Proxy ∷ Proxy Word32) (Proxy ∷ Proxy Word32) (Proxy ∷ Proxy Word32) (Proxy ∷ Proxy Word32)
  res ← fn 7
  kill
  43 T.@=? res

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
                        arguments = [(ConstantOperand (C.Int {C.integerBits = 64, C.integerValue = 10}), [])],
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
                  ( Do $ Ret (Just (ConstantOperand (C.Int 32 43))) []
                  )
              ]
          }
    ]

fn_test_example_jit ∷ T.TestTree
fn_test_example_jit = T.testCase "example module should jit function" $ do
  (imp, kill) ← jitWith (Config None) exampleModule dynamicImport
  Just fn ← importAs imp "_foo" (Proxy ∷ Proxy Word32) (Proxy ∷ Proxy Word32) (Proxy ∷ Proxy Word32) (Proxy ∷ Proxy Word32)
  res ← fn 7
  kill
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
