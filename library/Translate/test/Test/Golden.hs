module Test.Golden where

import qualified Data.ByteString as ByteString (readFile)
import qualified Juvix.Frontend.Parser as Parser
import Juvix.Frontend.Types (TopLevel)
import Juvix.Frontend.Types.Base (Header)
import Juvix.Library
import Juvix.Library.Test.Golden
import Test.Tasty
import qualified Juvix.Library.Feedback as Feedback
import qualified Prelude as P

type Pipeline = Feedback.FeedbackT [] P.String IO

parseContract :: FilePath -> IO (Either [Char] (Header TopLevel))
parseContract file = do
  Parser.prettyParse <$> ByteString.readFile file

parseTests :: IO TestTree
parseTests =
  testGroup "parse"
    <$> sequence
      [ discoverGoldenTestsParse "../../test/examples/positive",
        discoverGoldenTestsParse "../../test/examples/negative"
      ]

-- | Discover golden tests for input files with extension @.ju@ and output
-- files with extension @.parsed@.
discoverGoldenTestsParse ::
  -- | the directory in which to recursively look for golden tests
  FilePath ->
  IO TestTree
discoverGoldenTestsParse = discoverGoldenTests [".ju"] ".parsed" getGolden parseContract
