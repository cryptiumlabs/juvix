module Juvix.Backends.LLVM.JIT.Execution
  ( jit,
  )
where

import Control.Concurrent
import qualified Data.ByteString.Char8 as B
import Juvix.Backends.LLVM.JIT.Types
import Juvix.Library
import qualified LLVM.AST as AST
import LLVM.Context
import qualified LLVM.ExecutionEngine as EE
import LLVM.Module as Mod
import LLVM.PassManager
import LLVM.Target

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

-- Note: in order to allow this to return functions, a green thread is forked to retain module
-- state & handle function calls. This will not be cleaned up until the program quits. If we
-- end up calling `jit` a lot a different design might be in order.
jit ∷ (DynamicImport (a → IO b)) ⇒ Config → AST.Module → AST.Name → IO (a → IO b, IO ())
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
          B.putStrLn "getting execution engine"
          EE.withModuleInEngine executionEngine m $ \ee → do
            fref ← EE.getFunction ee name
            B.putStrLn "got fn"
            case fref of
              Just fn → do
                let hsFunc = castImport fn
                    loop = do
                      param ← readChan paramChan
                      case param of
                        Just p → do
                          res ← hsFunc p
                          writeChan resultChan res
                          loop
                        Nothing → return ()
                B.putStrLn "starting loop"
                loop
              Nothing → return ()
  let func param = writeChan paramChan (Just param) >> readChan resultChan
  return (func, writeChan paramChan Nothing)
