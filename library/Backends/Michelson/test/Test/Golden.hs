{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Test.Golden where

import qualified Juvix.Backends.Michelson as Michelson
import qualified Juvix.Core.Pipeline as Core
import Juvix.Library
import Juvix.Library.Test.Golden
import qualified Juvix.Pipeline as Pipeline
import Test.Tasty

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
      [ discoverGoldenTestsCompile "test/examples/positive/michelson",
        discoverGoldenTestsCompile "test/examples/negative/michelson"
      ]

typecheckTests :: IO TestTree
typecheckTests =
  testGroup "Michelson typecheck"
    <$> sequence
      [ discoverGoldenTestsTypecheck "test/examples/positive/michelson",
        discoverGoldenTestsTypecheck "test/examples/negative/michelson"
      ]

-- | Discover golden tests for input files with extension @.ju@ and output
-- files with extension @.typecheck@.
discoverGoldenTestsTypecheck ::
  -- | the directory in which to recursively look for golden tests
  FilePath ->
  IO TestTree
discoverGoldenTestsTypecheck (withJuvixRootPath -> p) = discoverGoldenTests [".ju"] ".typecheck" getGolden (expectSuccess . typecheck) p

typecheck file = do
  contract <- liftIO $ readFile file
  context <- Pipeline.parseWithLibs (withJuvixRootPath <$> libs) Michelson.BMichelson contract
  Pipeline.typecheck @Michelson.BMichelson context

-- | Discover golden tests for input files with extension @.ju@ and output
-- files with extension @.michelson@.
discoverGoldenTestsCompile ::
  -- | the directory in which to recursively look for golden tests
  FilePath ->
  IO TestTree
discoverGoldenTestsCompile (withJuvixRootPath -> p) = discoverGoldenTests [".ju"] ".michelson" getGolden (expectSuccess . compile) p
  where
    compile file = Michelson.compileMichelson =<< typecheck file