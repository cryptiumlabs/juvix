{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Test.Golden where

import qualified Juvix.Backends.Michelson as Michelson
import qualified Juvix.Core.ErasedAnn.Types as ErasedAnn
import qualified Juvix.Core.Pipeline as Core
import Juvix.Library
import Juvix.Library.Test.Golden
import qualified Juvix.Pipeline as Pipeline
import Test.Tasty
import Text.Pretty.Simple (pShowNoColor)

juvixRootPath :: FilePath
juvixRootPath = "../../../"

libs :: [[Char]]
libs = ["stdlib/Prelude.ju", "stdlib/Michelson.ju", "stdlib/MichelsonAlias.ju"]

withJuvixRootPath :: FilePath -> FilePath
withJuvixRootPath p = juvixRootPath <> p

top :: IO TestTree
top =
  testGroup "Michelson golden tests"
    <$> sequence
      [ typecheckTests,
        compileTests
      ]

compileTests :: IO TestTree
compileTests =
  testGroup "Michelson compile"
    <$> sequence
      [ compileTestPos "test/examples/positive/michelson",
        compileTestNeg "test/examples/negative/michelson"
      ]
  where
    compileTestPos = compileTest (expectSuccess . compile)
    compileTestNeg = compileTest (expectFailure . compile)
    compile file = Michelson.compileMichelson =<< typecheck file

typecheckTests :: IO TestTree
typecheckTests =
  testGroup "Michelson typecheck"
    <$> sequence
      [ typecheckTestPos "test/examples/positive/michelson",
        typecheckTestNeg "test/examples/negative/michelson"
      ]
  where
    typecheckTestPos = typecheckTest (expectSuccess . toNoQuotes typecheck)
    typecheckTestNeg = typecheckTest (expectFailure . toNoQuotes typecheck)

-- | Discover golden tests for input files with extension @.ju@ and output
-- files with extension @.typecheck@.
typecheckTest ::
  (FilePath -> IO NoQuotes) ->
  -- | the directory in which to recursively look for golden tests
  FilePath ->
  IO TestTree
typecheckTest f (withJuvixRootPath -> p) = discoverGoldenTests [".ju"] ".typecheck" getGolden f p

typecheck ::
  FilePath ->
  Pipeline.Pipeline
    ( ErasedAnn.AnnTermT
        Michelson.PrimTy
        Michelson.RawPrimVal
    )
typecheck file = do
  contract <- liftIO $ readFile file
  context <- Pipeline.parseWithLibs (withJuvixRootPath <$> libs) Michelson.BMichelson contract
  Pipeline.typecheck @Michelson.BMichelson context

-- | Discover golden tests for input files with extension @.ju@ and output
-- files with extension @.michelson@.
compileTest ::
  (Show b, Eq b, Read b) =>
  (FilePath -> IO b) ->
  -- | the directory in which to recursively look for golden tests
  FilePath ->
  IO TestTree
compileTest f (withJuvixRootPath -> p) = discoverGoldenTests [".ju"] ".michelson" getGolden f p
