module Test.Golden where

import qualified Data.ByteString as ByteString (readFile)
import Juvix.Library
import Juvix.Library.Test.Golden
import Test.Tasty
import qualified Juvix.Pipeline as Pipeline
import qualified Juvix.Backends.Plonk as Plonk
import Data.Curve.Weierstrass.BLS12381 (Fr)

--------------------------------------------------------------------------------
-- Parse contracts (Golden tests)
--------------------------------------------------------------------------------
-- TODO: Add translations as well?

parseContract :: FilePath -> IO ()
parseContract file = do
  (readFile file) >>= (\b -> Pipeline.parse b >=> Pipeline.typecheck @Plonk.BPlonk Fr)


parseTests :: IO TestTree
parseTests = testGroup "parse" <$> sequence
    [ discoverGoldenTestsParse "../../test/examples/positive" 
    -- TODO discoverGoldenTestsJuvix "test/examples/negative" parseTestNegative
    ]
-- | Discover golden tests for input files with extension @.ju@ and output
-- files with extension @.parsed@.
discoverGoldenTestsParse
  :: FilePath                 -- ^ the directory in which to recursively look for golden tests
  -> IO TestTree
discoverGoldenTestsParse = discoverGoldenTests [".ju"] ".circuit" getGolden parseContract


