{-# LANGUAGE ForeignFunctionInterface #-}

module Juvix.Backends.LLVM.JIT.Execution where

import qualified Data.ByteString.Char8 as B
import Foreign.Ptr (FunPtr, castFunPtr)
import Juvix.Backends.LLVM.JIT.Types
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

jit ∷ Config → Context → (EE.MCJIT → IO a) → IO a
jit config ctx = EE.withMCJIT ctx optlevel model ptrelim fastins
  where
    optlevel = convOptLevel (configOptimisationLevel config)

    model = Nothing -- code model (default)

    ptrelim = Nothing -- frame pointer elimination

    fastins = Nothing -- fast instruction selection, apparently not yet supported

passes ∷ Config → PassSetSpec
passes config = defaultCuratedPassSetSpec {optLevel = convOptLevel (configOptimisationLevel config)}

convOptLevel ∷ OptimisationLevel → Maybe Word
convOptLevel None = Nothing
convOptLevel O0 = pure 0
convOptLevel O1 = pure 1
convOptLevel O2 = pure 2
convOptLevel O3 = pure 3

runJIT ∷ Config → AST.Module → IO AST.Module
runJIT config mod = do
  withContext $ \context →
    jit config context $ \executionEngine → do
      initializeAllTargets
      withModuleFromAST context mod $ \m →
        withPassManager (passes config) $ \pm → do
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
