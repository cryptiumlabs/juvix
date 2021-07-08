{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Test.Golden where

import qualified Data.ByteString as ByteString (readFile)
import Data.Curve.Weierstrass.BLS12381 (Fr)
import qualified Juvix.Backends.Plonk as Plonk
import qualified Juvix.Core.Erased.Ann as ErasedAnn
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

top :: IO TestTree
top =
  testGroup "Plonk golden tests"
    <$> sequence
      [ typecheckTests,
        compileTests,
        hrTests
      ]

compileTests :: IO TestTree
compileTests =
  testGroup "Plonk compile"
    <$> sequence
      [ compileTestsPos "test/examples/positive/circuit",
        compileTestsNeg "test/examples/negative/circuit"
      ]
  where
    compileTestsPos = plonkGoldenTests ".circuit" (expectSuccess . typecheck)
    compileTestsNeg = plonkGoldenTests ".circuit" (expectFailure . typecheck)
    compile file = Plonk.compileCircuit <$> typecheck file

typecheckTests :: IO TestTree
typecheckTests =
  testGroup "Plonk typecheck"
    <$> sequence
      [ typecheckTestsPos "test/examples/positive/circuit",
        typecheckTestsNeg "test/examples/negative/circuit"
      ]
  where
    typecheckTestsPos = plonkGoldenTests ".typecheck" (expectSuccess . typecheck)
    typecheckTestsNeg = plonkGoldenTests ".typecheck" (expectFailure . typecheck)

typecheck file = do
  contract <- liftIO $ readFile file
  context <- Pipeline.parseWithLibs (withJuvixRootPath <$> libs) (Plonk.BPlonk @Fr) contract
  Pipeline.typecheck @(Plonk.BPlonk Fr) context

hrTests :: IO TestTree
hrTests =
  testGroup "Plonk HR"
    <$> sequence
      [ hrTestsPos "test/examples/positive/circuit",
        hrTestsNeg "test/examples/negative/circuit"
      ]
  where
    hrTestsPos = plonkGoldenTestsNoQuotes ".hr" (expectSuccess . toNoQuotes toHR)
    hrTestsNeg = plonkGoldenTestsNoQuotes ".hr" (expectFailure . toNoQuotes toHR)
    toHR file = do
      contract <- liftIO $ readFile file
      context <- Pipeline.parseWithLibs (withJuvixRootPath <$> libs) (Plonk.BPlonk @Fr) contract
      Pipeline.toHR context (Plonk.param @Fr)

plonkGoldenTestsNoQuotes = discoverGoldenTestsNoQuotes withJuvixRootPath

plonkGoldenTests ext f (withJuvixRootPath -> p) = discoverGoldenTests [".ju"] ext getGolden f p
