module Backends.LLVM2 where

import Juvix.Backends.LLVM.Codegen as Codegen
import Juvix.Backends.LLVM.JIT as JIT
import Juvix.Backends.LLVM.Net.EAC.Types as Types
import Juvix.Backends.LLVM.Net.Environment
import Juvix.Library
import LLVM.AST
-- -- import LLVM.AST.AddrSpace
-- -- import qualified LLVM.AST.Attribute as A
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Global as G
import qualified LLVM.AST.Instruction as I (function)
-- -- import qualified LLVM.AST.Linkage as L
-- -- import LLVM.AST.Name
import LLVM.AST.Type
-- -- import qualified LLVM.AST.Visibility as V
-- -- import LLVM.Context
-- -- import LLVM.ExecutionEngine
-- -- import LLVM.Module
import LLVM.Pretty

exampleModule2 ∷ LLVM.AST.Module
exampleModule2 =
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
                  [UnName 2 := Alloca Types.testList Nothing 0 []]
                  ( Do $ Ret (Just (LocalReference Types.testList (UnName 2))) []
                  )
              ]
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
                        arguments = [(ConstantOperand (C.Int {C.integerBits = 64, C.integerValue = 32}), [])],
                        functionAttributes = [],
                        metadata = []
                      },
                    UnName 2 := Alloca Types.testList Nothing 0 [],
                    -- UnName 3 := Call
                    --   { tailCallKind = Nothing,
                    --     I.function =
                    --       Right
                    --         ( ConstantOperand
                    --             ( C.GlobalReference
                    --                 (ptr $ FunctionType {resultType = voidTy
                    --                                     , argumentTypes = []
                    --                                     , isVarArg = False})
                    --                 (Name "test_function")
                    --             )
                    --         ),
                    --     callingConvention = CC.Fast,
                    --     returnAttributes = [],
                    --     arguments = [],
                    --     functionAttributes = [],
                    --     metadata = []
                    --   },
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

test_example_jit' = do
  let module' = Codegen.moduleAST runInitModule
  let newModule = module' {LLVM.AST.moduleDefinitions = LLVM.AST.moduleDefinitions module' <> LLVM.AST.moduleDefinitions exampleModule2}
  -- (link :: Word32 -> IO Word32, kill) <- JIT.jit (JIT.Config JIT.None) newModule "malloc"
  (imp, kill) ← jitWith (Config None) newModule dynamicImport
  Just fn ← importAs imp "test" (Proxy ∷ Proxy Word32) (Proxy ∷ Proxy Word32)
  res ← fn 7
  kill

test' ∷ MonadIO m ⇒ m ()
test' = putStr $ ppllvm (Codegen.moduleAST runInitModule)
