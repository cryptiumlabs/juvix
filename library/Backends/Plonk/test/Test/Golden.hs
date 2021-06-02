{-# LANGUAGE TypeApplications #-}
module Test.Golden where

import qualified Data.ByteString as ByteString (readFile)
import Juvix.Library
import Juvix.Library.Test.Golden
import Test.Tasty
import qualified Juvix.Pipeline as Pipeline
import Juvix.Pipeline (Pipeline)
import qualified Juvix.Backends.Plonk as Plonk
import Data.Curve.Weierstrass.BLS12381 (Fr)
import qualified Juvix.Core.ErasedAnn as ErasedAnn
import qualified Juvix.Library.Feedback as Feedback
import Text.Pretty.Simple (pPrint)

--------------------------------------------------------------------------------
-- Parse contracts (Golden tests)
--------------------------------------------------------------------------------


typecheckTests :: IO TestTree
typecheckTests = testGroup "Plonk typecheck" <$> sequence
    [ discoverGoldenTestsTypecheck "../../../test/examples/positive" 
    -- TODO discoverGoldenTestsJuvix "test/examples/negative" parseTestNegative
    ]
-- | Discover golden tests for input files with extension @.ju@ and output
-- files with extension @.parsed@.
discoverGoldenTestsTypecheck
  :: FilePath                 -- ^ the directory in which to recursively look for golden tests
  -> IO TestTree
discoverGoldenTestsTypecheck = discoverGoldenTests [".ju"] ".circuit" getGolden (expectSuccess . typecheck)
  where
    -- typecheck :: FilePath -> Pipeline (Plonk.FFAnnTerm Fr)
    typecheck file = do
      contract <- liftIO $ readFile file
      context <- Pipeline.parse (Plonk.BPlonk @(Plonk.BPlonk Fr)) contract
      Pipeline.typecheck @(Plonk.BPlonk Fr) context

    expectSuccess v = do
      feedback <- Feedback.runFeedbackT v
      case feedback of
        Feedback.Success msgs r -> do
          mapM_ pPrint msgs
          pure r
        Feedback.Fail msgs -> panic $ "Fail: " <> show msgs-- mapM_ pPrint msgs >> exitFailure




