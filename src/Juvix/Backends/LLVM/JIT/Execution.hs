module Juvix.Backends.LLVM.JIT.Execution where

import Control.Concurrent
import qualified Data.ByteString.Char8 as B
import Juvix.Backends.LLVM.JIT.Types
import Juvix.Library
import qualified LLVM.AST as AST
import LLVM.Context
import qualified LLVM.ExecutionEngine as EE
import LLVM.Module as Mod
import LLVM.PassManager

-- Need some datatypes to wrap since GHC doesn't yet support impredicative polymorphism.
data DynamicFunc where
  DynamicFunc ∷ ∀ a b. (DynamicImport (a → IO b), Typeable a, Typeable b) ⇒ (a → IO b) → DynamicFunc

data DynamicImportTypeProxy where
  DynamicImportTypeProxy ∷ ∀ a b. (DynamicImport (a → IO b), Typeable a, Typeable b) ⇒ Proxy a → Proxy b → DynamicImportTypeProxy

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

-- In order to allow this to return functions, a green thread is forked to retain module
-- state & handle function calls. The second element of the returned pair is an IO action to kill the thread.
jitWith ∷ ∀ a b. Config → AST.Module → (EE.ExecutableModule EE.MCJIT → IO ((a → IO b), IO ())) → IO (a → IO b, IO ())
jitWith config mod func = do
  paramChan ← newChan
  resultChan ← newChan
  void $ forkIO $ withContext $ \context →
    runJIT config context $ \executionEngine → do
      withModuleFromAST context mod $ \m →
        withPassManager (passes config) $ \pm → do
          -- optimise module
          _ ← runPassManager pm m
          -- convert to llvm assembly
          s ← moduleLLVMAssembly m
          B.putStrLn s
          B.putStrLn "getting execution engine"
          EE.withModuleInEngine executionEngine m $ \ee → do
            B.putStrLn "got execution engine"
            (handler, terminate) ← func ee
            let loop = do
                  param ← readChan paramChan
                  case param of
                    Just p → do
                      res ← handler p
                      writeChan resultChan res
                      loop
                    Nothing → terminate
            loop
  let func param = writeChan paramChan (Just param) >> readChan resultChan
  return (func, writeChan paramChan Nothing)

importAs ∷ ((AST.Name, DynamicImportTypeProxy) → IO (Maybe DynamicFunc)) → (∀ a b. (DynamicImport (a → IO b), Typeable a, Typeable b) ⇒ AST.Name → Proxy a → Proxy b → IO (Maybe (a → IO b)))
importAs imp name (Proxy ∷ Proxy a) (Proxy ∷ Proxy b) = do
  maybeFunc ← imp (name, DynamicImportTypeProxy (Proxy ∷ Proxy a) (Proxy ∷ Proxy b))
  case maybeFunc of
    Just (DynamicFunc (f ∷ x → IO y)) →
      case (eqT ∷ Maybe (x :~: a), eqT ∷ Maybe (y :~: b)) of
        -- This should always be true but GHC can't prove it, blah.
        (Just Refl, Just Refl) → pure (pure f)
        _ → pure Nothing
    Nothing → pure Nothing

dynamicImport ∷ EE.ExecutableModule EE.MCJIT → IO ((AST.Name, DynamicImportTypeProxy) → IO (Maybe DynamicFunc), IO ())
dynamicImport ee = do
  actions ← newMVar []
  let terminate = mapM_ (\x → x) =<< readMVar actions
      handler (name, DynamicImportTypeProxy (Proxy ∷ Proxy a) (Proxy ∷ Proxy b)) = do
        fref ← EE.getFunction ee name
        case fref of
          Just fn → do
            paramChan ← newChan
            resultChan ← newChan
            let hsFunc = castImport fn ∷ a → IO b
                loop = do
                  param ← readChan paramChan
                  case param of
                    Just p → do
                      res ← hsFunc p
                      writeChan resultChan res
                      loop
                    Nothing → return ()
            modifyMVar_ actions (pure . (:) (writeChan paramChan Nothing))
            void $ forkIO loop
            let func ∷ a → IO b
                func param = writeChan paramChan (Just param) >> readChan resultChan
            pure (Just (DynamicFunc func))
          Nothing →
            pure Nothing
  pure (handler, terminate)
