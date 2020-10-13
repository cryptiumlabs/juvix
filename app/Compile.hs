module Compile where

import qualified Data.Text.IO as T
import qualified Juvix.Pipeline as Pipeline
import qualified Juvix.Backends.Michelson.Compilation as M
import qualified Juvix.Backends.Michelson.Parameterisation as Param
import qualified Juvix.Core.ErasedAnn as ErasedAnn
import Juvix.Library
import Options

typecheck ::
  FilePath -> Backend -> IO (ErasedAnn.AnnTerm Param.PrimTy Param.PrimVal)
typecheck fin Michelson = do
  core <- Pipeline.toCore [fin]
  case core of
    Right ctx -> do
      print ctx
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
