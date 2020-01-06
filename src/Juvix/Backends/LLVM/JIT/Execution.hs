module Juvix.Backends.LLVM.JIT.Execution where

import Control.Concurrent
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Short as BS
import Data.IORef
import qualified Data.Map.Strict as Map
import Foreign.Ptr
import Juvix.Backends.LLVM.JIT.Types
import Juvix.Library
import qualified LLVM.AST as AST
import qualified LLVM.CodeGenOpt as CodeGenOpt
import qualified LLVM.CodeModel as CodeModel
import LLVM.Context
import qualified LLVM.ExecutionEngine as EE
import LLVM.Internal.OrcJIT.CompileLayer
import LLVM.Linking
import LLVM.Module as Mod
import LLVM.Module
import LLVM.OrcJIT
import LLVM.PassManager
import qualified LLVM.Relocation as Reloc
import LLVM.Target

-- Need some datatypes to wrap since GHC doesn't yet support impredicative polymorphism.
data DynamicFunc where
  DynamicFunc ∷
    ∀ a b c.
    (DynamicImport a (b → IO c), Typeable a, Typeable b, Typeable c) ⇒
    (b → IO c) →
    DynamicFunc

data DynamicImportTypeProxy where
  DynamicImportTypeProxy ∷
    ∀ a b c.
    (DynamicImport a (b → IO c), Typeable a, Typeable b, Typeable c) ⇒
    Proxy a →
    Proxy b →
    Proxy c →
    DynamicImportTypeProxy

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

orcJitWith ∷
  ∀ a b.
  Config →
  AST.Module →
  ( (AST.Name → IO (Maybe (FunPtr ()))) →
    IO ((a → IO b), IO ())
  ) →
  IO (a → IO b, IO ())
orcJitWith config mod func = do
  paramChan ← newChan
  resultChan ← newChan
  endChan ← newChan
  void $ forkIO $ withContext $ \context → do
    resolvers ← newIORef Map.empty
    withModuleFromAST context mod $ \m → do
      putText "got module"
      withHostTargetMachine Reloc.PIC CodeModel.Default CodeGenOpt.Default $ \tm → do
        putText "got target machine"
        withExecutionSession $ \es → do
          putText "got execution session"
          withModuleKey es $ \k → do
            putText "got module key"
            withObjectLinkingLayer es (\k → fmap (\rs → rs Map.! k) (readIORef resolvers)) $ \objectLayer → do
              putText "got linking layer"
              withIRCompileLayer objectLayer tm $ \compileLayer → do
                putText "got compile layer"
                withSymbolResolver es (SymbolResolver (\sym → findSymbol compileLayer sym True)) $ \resolver → do
                  modifyIORef' resolvers (Map.insert k resolver)
                  putText "got symbol resolver"
                  withModule compileLayer k m $ do
                    putText "got module"
                    (handler, terminate) ← func $ \(AST.Name name) → do
                      mainSymbol ← mangleSymbol compileLayer name
                      sym ← findSymbol compileLayer mainSymbol True
                      putText (show sym)
                      pure $ case sym of
                        Right (JITSymbol f _) → pure (castPtrToFunPtr (wordPtrToPtr f))
                        _ → Nothing
                    let loop = do
                          param ← readChan paramChan
                          case param of
                            Just p → do
                              res ← handler p
                              writeChan resultChan res
                              loop
                            Nothing → terminate >> writeChan endChan ()
                    loop
  let func param = writeChan paramChan (Just param) >> readChan resultChan
  return (func, writeChan paramChan Nothing >> readChan endChan)

-- In order to allow this to return functions, a green thread is forked to retain module
-- state & handle function calls. The second element of the returned pair is an IO action to kill the thread.
mcJitWith ∷ ∀ a b. Config → AST.Module → ((AST.Name → IO (Maybe (FunPtr ()))) → IO ((a → IO b), IO ())) → IO (a → IO b, IO ())
mcJitWith config mod func = do
  paramChan ← newChan
  resultChan ← newChan
  endChan ← newChan
  void $ forkIO $ withContext $ \context →
    runJIT config context $ \executionEngine → do
      putText "calling withModuleFromAST"
      withModuleFromAST context mod $ \m → do
        putText "calling withPassManager"
        withPassManager (passes config) $ \pm → do
          -- optimise module
          _ ← runPassManager pm m
          -- convert to llvm assembly
          s ← moduleLLVMAssembly m
          B.putStrLn "getting execution engine"
          EE.withModuleInEngine executionEngine m $ \ee → do
            B.putStrLn "got execution engine"
            (handler, terminate) ← func (EE.getFunction ee)
            let loop = do
                  param ← readChan paramChan
                  case param of
                    Just p → do
                      res ← handler p
                      writeChan resultChan res
                      loop
                    Nothing → terminate >> writeChan endChan ()
            loop
  let func param = writeChan paramChan (Just param) >> readChan resultChan
  return (func, writeChan paramChan Nothing >> readChan endChan)

importAs ∷
  ((AST.Name, DynamicImportTypeProxy) → IO (Maybe DynamicFunc)) →
  ( ∀ a b c.
    (DynamicImport a (b → IO c), Typeable a, Typeable b, Typeable c) ⇒
    AST.Name →
    Proxy a →
    Proxy b →
    Proxy c →
    IO (Maybe (b → IO c))
  )
importAs imp name (Proxy ∷ Proxy a) (Proxy ∷ Proxy b) (Proxy ∷ Proxy c) = do
  maybeFunc ← imp (name, DynamicImportTypeProxy (Proxy ∷ Proxy a) (Proxy ∷ Proxy b) (Proxy ∷ Proxy c))
  case maybeFunc of
    Just (DynamicFunc (f ∷ x → IO y)) →
      case (eqT ∷ Maybe (x :~: b), eqT ∷ Maybe (y :~: c)) of
        -- This should always be true but GHC can't prove it, blah.
        (Just Refl, Just Refl) → pure (pure f)
        _ → pure Nothing
    Nothing → pure Nothing

dynamicImport ∷
  (AST.Name → IO (Maybe (FunPtr ()))) →
  IO ((AST.Name, DynamicImportTypeProxy) → IO (Maybe DynamicFunc), IO ())
dynamicImport lookup = do
  actions ← newMVar []
  let terminate = mapM_ (\x → x) =<< readMVar actions
      handler (name, DynamicImportTypeProxy (Proxy ∷ Proxy a) (Proxy ∷ Proxy b) (Proxy ∷ Proxy c)) = do
        putText ("Importing: " <> show name)
        fref ← lookup name
        case fref of
          Just fn → do
            paramChan ← newChan
            resultChan ← newChan
            termChan ← newChan
            let hsFunc = castImport fn ∷ b → IO c
                loop = do
                  param ← readChan paramChan
                  case param of
                    Just p → do
                      res ← hsFunc p
                      writeChan resultChan res
                      loop
                    Nothing → writeChan termChan () >> pure ()
            modifyMVar_ actions (pure . (:) (writeChan paramChan Nothing >> readChan termChan))
            void $ forkIO loop
            let func ∷ b → IO c
                func param = writeChan paramChan (Just param) >> readChan resultChan
            pure (Just (DynamicFunc func))
          Nothing →
            pure Nothing
  pure (handler, terminate)
