module Juvix.Backends.LLVM.JIT where

import qualified Data.ByteString.Char8 as B
import Foreign.Ptr (FunPtr, castFunPtr)
import Juvix.Library
import qualified LLVM.AST as AST
import LLVM.Context
import qualified LLVM.ExecutionEngine as EE
import LLVM.Module as Mod
import LLVM.PassManager
import LLVM.Target

foreign import ccall "dynamic" haskFun ∷ FunPtr (IO Double) → (IO Double)

run ∷ FunPtr a → IO Double
run fn = haskFun (castFunPtr fn ∷ FunPtr (IO Double))

jit ∷ Context → (EE.MCJIT → IO a) → IO a
jit c = EE.withMCJIT c optlevel model ptrelim fastins
  where
    optlevel = Just 3 -- optimization level

    model = Nothing -- code model ( Default )

    ptrelim = Nothing -- frame pointer elimination

    fastins = Nothing -- fast instruction selection

passes ∷ PassSetSpec
passes = defaultCuratedPassSetSpec {optLevel = Just 3}

runJIT ∷ AST.Module → IO AST.Module
runJIT mod = do
  withContext $ \context →
    jit context $ \executionEngine → do
      initializeAllTargets
      withModuleFromAST context mod $ \m →
        withPassManager passes $ \pm → do
          -- optimise module
          _ ← runPassManager pm m
          -- fetch the optimised ast
          optmod ← moduleAST m
          -- convert to llvm assembly
          s ← moduleLLVMAssembly m
          B.putStrLn s
          EE.withModuleInEngine executionEngine m $ \ee → do
            mainfn ← EE.getFunction ee "main"
            case mainfn of
              Just fn → do
                res ← run fn
                putStrLn $ "Evaluated to: " ++ show res
              Nothing → return ()
          -- Return the optimized module
          return optmod
