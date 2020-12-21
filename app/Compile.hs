{-# LANGUAGE LiberalTypeSynonyms #-}

module Compile where

import qualified Data.HashMap.Strict as HM
import qualified Data.Text.IO as T
import qualified Juvix.Backends.Michelson.Compilation as M
import qualified Juvix.Backends.Michelson.Parameterisation as Param
import qualified Juvix.Core.ErasedAnn as ErasedAnn
import qualified Juvix.Core.HR as HR
import qualified Juvix.Core.IR as IR
import qualified Juvix.Core.Pipeline as CorePipeline
import qualified Juvix.Core.Translate as Translate
import qualified Juvix.FrontendContextualise.InfixPrecedence.Environment as FE
import Juvix.Library
import qualified Juvix.Library.Usage as Usage
import qualified Juvix.Pipeline as Pipeline
import qualified Michelson.Untyped as Untyped
import Options
import Types

parse :: FilePath -> IO FE.FinalContext
parse fin = do
  core <- Pipeline.toCore ["stdlib/Prelude.ju", "stdlib/Michelson.ju", "stdlib/MichelsonAlias.ju", fin]
  case core of
    Right ctx -> pure ctx
    Left err -> do
      T.putStrLn (show err)
      exitFailure

globalsToMap = HM.fromList . map (\x -> (IR.globalName x, x))

typecheck ::
  FilePath -> Backend -> IO (ErasedAnn.AnnTerm Param.PrimTy Param.PrimVal)
typecheck fin Michelson = do
  ctx <- parse fin
  let res = Pipeline.contextToCore ctx Param.michelson
  case res of
    Right globals -> do
      case filter (\x -> case x of (IR.GFunction (IR.Function "entry" _ _ _)) -> True; _ -> False) globals of
        [] -> do
          T.putStrLn "No entrypoint found"
          exitFailure
        (IR.GFunction (IR.Function name usage ty (IR.FunClause [] term :| []))) : [] -> do
          (res, _) <- exec (CorePipeline.coreToAnn term (IR.globalToUsage usage) ty) Param.michelson (globalsToMap globals)
          case res of
            Right r -> pure r
            Left err -> do
              T.putStrLn (show err)
              exitFailure
          exitSuccess
    Left err -> do
      print err
      exitFailure
typecheck _ _ = exitFailure

compile :: FilePath -> FilePath -> Backend -> IO ()
compile fin fout backend = do
  term <- typecheck fin backend
  let (res, _logs) = M.compileContract $ CorePipeline.toRaw term
  case res of
    Right c -> do
      T.writeFile fout (M.untypedContractToSource (fst c))
    Left err -> do
      T.putStrLn (show err)
      exitFailure
