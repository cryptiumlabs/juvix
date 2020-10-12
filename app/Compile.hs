module Compile where

import Control.Monad.IO.Class
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Juvix.Backends.Michelson.Compilation as M
import qualified Juvix.Backends.Michelson.Parameterisation as Param
import qualified Juvix.Core as Core
import qualified Juvix.Core.Erasure as Erasure
import qualified Juvix.Core.HR as HR
import Juvix.Library
import Options
import Types

execMichelson ::
  Core.T ->
  HR.Term Param.PrimTy Param.PrimVal ->
  HR.Term Param.PrimTy Param.PrimVal ->
  Exec Param.PrimTy Param.PrimVal Param.CompErr
execMichelson usage term ty =
  -- TODO add globals (from builtins, etc)                         ↓
  liftIO (exec (Core.typecheckErase term usage ty) Param.michelson mempty)

typecheck ::
  FilePath -> Backend -> IO (Erasure.Term Param.PrimTy Param.PrimVal)
typecheck fin Michelson = do
  source <- readFile fin
  let parsed = HR.generateParser Param.michelson (T.unpack source)
  case parsed of
    Just (HR.Elim (HR.Ann usage term ty _)) -> do
      erased <- execMichelson usage term ty
      case erased of
        (Right term, _) ->
          pure term
        other -> do
          T.putStrLn (show other)
          exitFailure
    err -> do
      T.putStrLn (show err)
      exitFailure
typecheck _ _ = exitFailure

compile :: FilePath -> FilePath -> Backend -> IO ()
compile fin fout backend = do
  _term <- typecheck fin backend
  -- TODO: Annotated version.
  let (res, _logs) = M.compileContract undefined
  case res of
    Left err -> do
      T.putStrLn (show err)
      exitFailure
    Right c -> do
      T.writeFile fout (M.untypedContractToSource (fst c))
