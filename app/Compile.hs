{-# LANGUAGE LiberalTypeSynonyms #-}

module Compile where

import qualified Data.HashMap.Strict as HM
import Juvix.Core.Parameterisation
import Juvix.Core.IR.Types.Base
import qualified Data.Text.IO as T
import Juvix.Core.FromFrontend as FF
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

typecheck ::
  FilePath -> Backend -> IO (ErasedAnn.AnnTerm Param.PrimTy Param.PrimVal)
typecheck fin Michelson = do
  ctx <- parse fin
  let res = Pipeline.contextToCore ctx Param.michelson
  case res of
    Right globals -> do
      let globalDefs = HM.mapMaybe (\case (CoreDef g) -> pure g; _ -> Nothing) $ FF.defs globals
      case HM.elems $ HM.filter (\x -> case x of (IR.GFunction (IR.Function ("TopLevel" :| [_, "main"]) _ _ _)) -> True; _ -> False) globalDefs of
        [] -> do
          print globalDefs
          T.putStrLn "No main function found"
          exitFailure
        (IR.GFunction (IR.Function name usage ty (IR.FunClause [] term :| []))) : [] -> do
          exitSuccess
          (res, _) <- exec (CorePipeline.coreToAnn term (IR.globalToUsage usage) ty) Param.michelson (HM.map unsafeEvalGlobal globalDefs)
          case res of
            Right r -> pure r
            Left err -> do
              T.putStrLn (show err)
              exitFailure
          exitSuccess
        somethingElse -> do
          print somethingElse
          exitFailure
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

unsafeEvalGlobal :: IR.RawGlobal Param.PrimTy Param.RawPrimVal -> GlobalWith Value' IR.NoExt Param.PrimTy (Juvix.Core.Parameterisation.TypedPrim Param.PrimTy Param.RawPrimVal)
unsafeEvalGlobal g =
  case g of
    _ -> undefined
    -- GAbstract (Abstract n u t) -> GAbstract (Abstract n u (unsafeEval t))

unsafeEval = (\(Right x) -> x) . IR.evalTerm


