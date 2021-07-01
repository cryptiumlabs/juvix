{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Test.Golden where

import qualified Data.ByteString as ByteString (readFile)
import qualified Juvix.Backends.LLVM as LLVM
import qualified Juvix.Core.Erased.Ann as ErasedAnn
import Juvix.Library
import qualified Juvix.Library.Feedback as Feedback
import Juvix.Library.Test.Golden
import Juvix.Pipeline (Pipeline)
import qualified Juvix.Pipeline as Pipeline
import Test.Tasty

--------------------------------------------------------------------------------
-- Parse contracts (Golden tests)
--------------------------------------------------------------------------------

juvixRootPath :: FilePath
juvixRootPath = "../../../"

libs :: [[Char]]
libs = ["stdlib/Prelude.ju", "stdlib/LLVM.ju"]

withJuvixRootPath :: FilePath -> FilePath
withJuvixRootPath p = juvixRootPath <> p

top :: IO TestTree
top =
  testGroup "LLVM golden tests"
    <$> sequence
      [ typecheckTests,
        compileTests
      ]

compileTests :: IO TestTree
compileTests =
  testGroup "LLVM compile"
    <$> sequence
      [ discoverGoldenTestsCompile "test/examples/positive/llvm",
        discoverGoldenTestsCompile "test/examples/negative/llvm"
      ]

typecheckTests :: IO TestTree
typecheckTests =
  testGroup "LLVM typecheck"
    <$> sequence
      [ discoverGoldenTestsTypecheck "test/examples/positive/llvm",
        discoverGoldenTestsTypecheck "test/examples/negative/llvm"
      ]

-- | Discover golden tests for input files with extension @.ju@ and output
-- files with extension @.typecheck@.
discoverGoldenTestsTypecheck ::
  -- | the directory in which to recursively look for golden tests
  FilePath ->
  IO TestTree
discoverGoldenTestsTypecheck (withJuvixRootPath -> p) = discoverGoldenTests [".ju"] ".typecheck" getGolden (expectSuccess . typecheck) p

typecheck ::
  FilePath ->
  Feedback.FeedbackT
    []
    [Char]
    IO
    (ErasedAnn.AnnTermT LLVM.PrimTy LLVM.RawPrimVal)
typecheck file = do
  contract <- liftIO $ readFile file
  context <- Pipeline.parseWithLibs (withJuvixRootPath <$> libs) LLVM.BLLVM contract
  Pipeline.typecheck @LLVM.BLLVM context

-- | Discover golden tests for input files with extension @.ju@ and output
-- files with extension @.llvm@.
discoverGoldenTestsCompile ::
  -- | the directory in which to recursively look for golden tests
  FilePath ->
  IO TestTree
discoverGoldenTestsCompile (withJuvixRootPath -> p) = discoverGoldenTests [".ju"] ".llvm" getGolden (expectSuccess . compile) p
  where
    compile file = LLVM.compileProgram . ErasedAnn.toRaw =<< typecheck file
