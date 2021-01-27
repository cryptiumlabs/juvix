{-# LANGUAGE LiberalTypeSynonyms #-}

module Compile where

import qualified Data.Text.IO as T
import qualified Juvix.Backends.Michelson.Compilation as M
import qualified Juvix.Backends.Michelson.Parameterisation as Param
import qualified Juvix.Core.ErasedAnn as ErasedAnn
import qualified Juvix.Core.HR as HR
import qualified Juvix.Core.Pipeline as CorePipeline
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
  FilePath -> Backend -> IO (ErasedAnn.AnnTerm Param.PrimTy Param.PrimValHR)
typecheck fin Michelson = do
  ctx <- parse fin
  let res = Pipeline.contextToCore ctx Param.michelson
  case res of
    Right globals -> do
      print globals
      -- TODO: Lookup the entrypoint then run `coreToAnn` as below.
      exitSuccess
    Left err -> do
      print err
      exitFailure
typecheck _ _ = exitFailure

typecheck' ::
  FilePath -> Backend -> IO (ErasedAnn.AnnTerm Param.PrimTy Param.PrimValHR)
typecheck' _ _ = do
  -- These terms are fake for now.
  let usage :: Usage.T
      usage = Usage.Omega
      ann :: HR.Term Param.PrimTy Param.RawPrimVal
      -- FIXME: replace prims with Returns
      ann = HR.Pi (Usage.SNat 1) "_" (HR.PrimTy (Param.PrimTy (Untyped.Type Untyped.TInt ""))) (HR.PrimTy (Param.PrimTy (Untyped.Type (Untyped.TPair "" "" (Untyped.Type Untyped.TInt "") (Untyped.Type (Untyped.TList (Untyped.Type Untyped.TOperation "")) "")) "")))
      term :: HR.Term Param.PrimTy Param.RawPrimVal
      term = HR.Lam "x" (HR.Elim (HR.App (HR.App (HR.Ann (Usage.SNat 1) (HR.Prim (Param.Inst (Untyped.PAIR "" "" "" ""))) ann 1) (HR.Elim (HR.Var "x"))) (HR.Prim (Param.Constant Untyped.ValueNil))))
      globals = mempty
  (res, _) <- exec (CorePipeline.coreToAnn term usage ann) Param.michelson globals
  case res of
    Right r -> pure r
    Left err -> do
      T.putStrLn (show err)
      exitFailure

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
