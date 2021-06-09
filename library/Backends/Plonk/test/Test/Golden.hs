{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Test.Golden where

import qualified Data.ByteString as ByteString (readFile)
import Data.Curve.Weierstrass.BLS12381 (Fr)
import qualified Juvix.Backends.Plonk as Plonk
import qualified Juvix.Core.ErasedAnn as ErasedAnn
import Juvix.Library
import qualified Juvix.Library.Feedback as Feedback
import Juvix.Library.Test.Golden
import Juvix.Pipeline (Pipeline)
import qualified Juvix.Pipeline as Pipeline
import Test.Orphan
import Test.Tasty
import Text.Pretty.Simple (pPrint)

--------------------------------------------------------------------------------
-- Parse contracts (Golden tests)
--------------------------------------------------------------------------------

juvixRootPath :: FilePath
juvixRootPath = "../../../"

libs :: IsString a => [a]
libs = ["stdlib/Prelude.ju", "stdlib/Circuit.ju"]

withJuvixRootPath :: FilePath -> FilePath
withJuvixRootPath p = juvixRootPath <> p

top =
  testGroup "Plonk golden tests"
    <$> sequence
      [ typecheckTests,
        compileTests
      ]

compileTests :: IO TestTree
compileTests =
  testGroup "Plonk compile"
    <$> sequence
      [ discoverGoldenTestsCompile "test/examples/positive/circuit",
        discoverGoldenTestsCompile "test/examples/negative/circuit"
      ]

typecheckTests :: IO TestTree
typecheckTests =
  testGroup "Plonk typecheck"
    <$> sequence
      [ discoverGoldenTestsTypecheck "test/examples/positive/circuit",
        discoverGoldenTestsTypecheck "test/examples/negative/circuit"
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
  context <- Pipeline.parseWithLibs (withJuvixRootPath <$> libs) (Plonk.BPlonk @Fr) contract
  Pipeline.typecheck @(Plonk.BPlonk Fr) context

-- | Discover golden tests for input files with extension @.ju@ and output
-- files with extension @.circuit@.
discoverGoldenTestsCompile ::
  -- | the directory in which to recursively look for golden tests
  FilePath ->
  IO TestTree
discoverGoldenTestsCompile (withJuvixRootPath -> p) = discoverGoldenTests [".ju"] ".circuit" getGolden (expectSuccess . compile) p
  where
    compile file = Plonk.compileCircuit <$> typecheck file
