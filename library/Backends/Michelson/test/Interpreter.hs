module Interpreter where

import Juvix.Library
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T
import Test.Tasty.Golden     (findByExtension)
import System.Directory   (doesFileExist)
import qualified Juvix.Backends.Michelson.DSL.Interpret as Interpret
import qualified Juvix.Backends.Michelson as Michelson
import qualified Juvix.Backends.Michelson.Parameterisation as Param
import Juvix.Core.Types
import Data.Either (isRight)
import qualified Juvix.Core.ErasedAnn as ErasedAnn
import qualified Juvix.Pipeline as Pipeline
import Control.Arrow (left)
import qualified Michelson.Untyped.Aliases as Alias
import qualified Michelson.Interpret as Interpret
import qualified Juvix.Backends.Michelson.Compilation as M
import qualified Juvix.Library.Feedback as Feedback
import qualified Juvix.Core.Pipeline as CorePipeline
import Prelude (String)

compile :: ErasedAnn.AnnTerm Param.PrimTy Param.PrimValHR -> Either Param.CompilationError Alias.Contract
compile term  = do
  let (res, _logs) = M.compileContract $ CorePipeline.toRaw term
  fst <$> res

-- | 
-- 'expectSuccess' automatically fails on a 'Left' result and
-- 'expectFailure' automatically fails on a 'Right' result.
expectFailure, expectSuccess
  :: (Show a, Show b)
  => (FilePath -> IO (Either a b)) -- ^ The IO action to run that will give either failure or success
  -> FilePath
  -> IO T.TestTree
expectSuccess action file = (\v -> T.testCase file . T.assertBool (show $ fromLeft (panic "Expected Left!") v) $ isRight v) <$> withPrint (action file)
expectFailure action file = (\v -> T.testCase file . T.assertBool (show $ fromRight (panic "Expected Right!") v) $ isLeft v) <$> withPrint (action file)
  where
    withPrint :: Show a => IO a -> IO a
    withPrint m = do
      a <- m
      print a
      return a

-- Path relative to library/Backends
readJuvixExamples :: IO [FilePath]
readJuvixExamples = findByExtension [".ju"] "../../test/examples"

data PErr = 
           TypecheckErr [String]
          | CompileErr Param.CompilationError
          | InterpretErr Interpret.InterpretError
    deriving (Show)

pipelineFromFile ::
  forall a b.
  (Show a, Pipeline.HasBackend b) =>
  FilePath ->
  b ->
  (forall b. Pipeline.HasBackend b => b -> Text -> Pipeline.Pipeline a) ->
  Pipeline.Pipeline a
pipelineFromFile fin b f = liftIO (readFile fin) >>= f b 

compileJuvixFile :: FilePath -> IO (Either PErr Alias.Contract)
compileJuvixFile fpath = do
  r <- Feedback.runFeedbackT $
    pipelineFromFile 
      fpath 
      Michelson.BMichelson 
      (\b -> Pipeline.parse b >=> Pipeline.typecheck @Michelson.BMichelson)
  case r of
    Feedback.Success _msgs r -> pure $ left CompileErr (compile r)
    Feedback.Fail err -> pure $ Left (TypecheckErr err)
  
interpretJuvixFile :: Alias.Contract -> Either PErr Interpret.InterpretResult
interpretJuvixFile = left InterpretErr . Interpret.dummyInterpretContract

top :: IO T.TestTree
top = do
  contractFiles <- readJuvixExamples
  tests <- traverse (expectSuccess action) contractFiles
  pure $ T.testGroup "Michelson Interpreter" tests
  where
    action f = do
      cE <- compileJuvixFile f
      case cE of
        Left err -> pure $ Left err
        Right c -> pure $ interpretJuvixFile c
