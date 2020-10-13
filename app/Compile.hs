module Compile where

import qualified Data.Text.IO as T
import qualified Michelson.Untyped.Type as Untyped
import qualified Juvix.Core.Usage as Usage
import qualified Juvix.Pipeline as Pipeline
import qualified Juvix.Core.Pipeline as CorePipeline
import qualified Juvix.Backends.Michelson.Compilation as M
import qualified Juvix.Core.HR as HR
import qualified Juvix.Backends.Michelson.Parameterisation as Param
import qualified Juvix.Core.ErasedAnn as ErasedAnn
import Juvix.Library
import Options
import Types

typecheck ::
  FilePath -> Backend -> IO (ErasedAnn.AnnTerm Param.PrimTy Param.PrimVal)
typecheck fin Michelson = do
  core <- Pipeline.toCore ["stdlib/Prelude.ju", "stdlib/Michelson.ju", fin]
  case core of
    Right ctx -> do
      print ctx
      -- Need to transform Context.T into core + globals.
      -- These terms are fake for now.
      let term :: HR.Term Param.PrimTy Param.PrimVal
          term = HR.Lam "x" (HR.Elim (HR.Var "x"))
          usage :: Usage.T
          usage = Usage.Omega
          ann :: HR.Term Param.PrimTy Param.PrimVal
          ann = HR.Pi (Usage.SNat 1) "_" (HR.PrimTy (Param.PrimTy (Untyped.Type Untyped.TInt ""))) (HR.PrimTy (Param.PrimTy (Untyped.Type Untyped.TInt "")))
          globals = mempty
      (res, _) <- exec (CorePipeline.coreToAnn term usage ann) Param.michelson globals
      case res of
        Right r -> pure r
        Left err -> do
          T.putStrLn (show err)
          exitFailure
    Left err -> do
      T.putStrLn (show err)
      exitFailure
typecheck _ _ = exitFailure

compile :: FilePath -> FilePath -> Backend -> IO ()
compile fin fout backend = do
  _term <- typecheck fin backend
  let (res, _logs) = M.compileContract _term
  case res of
    Right c -> do
      T.writeFile fout (M.untypedContractToSource (fst c))
    Left err -> do
      T.putStrLn (show err)
      exitFailure
