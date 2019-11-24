{-# LANGUAGE ForeignFunctionInterface #-}

module Juvix.Backends.LLVM.JIT.Execution
  ( jit,
  )
where

import Control.Concurrent
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

foreign import ccall "dynamic" haskFun ∷ FunPtr (Word32 → IO Word32) → (Word32 → IO Word32)

run ∷ FunPtr a → (Word32 → IO Word32)
run fn = haskFun (castFunPtr fn ∷ FunPtr (Word32 → IO Word32))

runJIT ∷ Config → Context → (EE.MCJIT → IO a) → IO a
runJIT config ctx = EE.withMCJIT ctx optlevel model ptrelim fastins
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

jit ∷ Config → AST.Module → AST.Name → IO (Word32 → IO Word32)
jit config mod name = do
  paramChan ← newChan
  resultChan ← newChan
  void $ forkIO $ withContext $ \context →
    runJIT config context $ \executionEngine → do
      initializeAllTargets
      withModuleFromAST context mod $ \m →
        withPassManager (passes config) $ \pm → do
          -- optimise module
          _ ← runPassManager pm m
          -- convert to llvm assembly
          s ← moduleLLVMAssembly m
          B.putStrLn s
          EE.withModuleInEngine executionEngine m $ \ee → do
            fref ← EE.getFunction ee name
            case fref of
              Just fn → forever $ do
                param ← readChan paramChan
                res ← run fn param
                writeChan resultChan res
              Nothing → return ()
  return $ \param → writeChan paramChan param >> readChan resultChan
