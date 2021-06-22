module Test.Golden where

import qualified Data.ByteString as ByteString (readFile)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Juvix.Frontend.Parser as Parser
import Juvix.Frontend.Types (TopLevel)
import Juvix.Frontend.Types.Base (Header)
import Juvix.Library
import Juvix.Library.Test.Golden
import Test.Tasty
import qualified Juvix.ToCore.FromFrontend.Transform.HR as HR
import qualified Juvix.Library.Sexp.Parser as SexpParser
import qualified Juvix.Library.Feedback as Feedback
import qualified Juvix.ToCore.Types
import qualified Juvix.Core.Parameterisation
import qualified Juvix.Core.HR.Types

import qualified Prelude as P

type Pipeline = Feedback.FeedbackT [] P.String IO
--------------------------------------------------------------------------------
-- Parse contracts (Golden tests)
--------------------------------------------------------------------------------


xx :: (Show primTy, Show primVal,
       HasThrow
                      "fromFrontendError"
                      (Juvix.ToCore.Types.Error primTy primVal)
                      (Feedback.FeedbackT [] P.String IO),
      HasReader
                      "param"
                      (Juvix.Core.Parameterisation.Parameterisation primTy primVal)
                      (Feedback.FeedbackT [] P.String IO),
      HasState
                      "coreSigs"
                      (Juvix.ToCore.Types.CoreSigsHR primTy primVal)
                      (Feedback.FeedbackT [] P.String IO)
  ) => ByteString -> Pipeline (Either [Char] (Juvix.Core.HR.Types.Term primTy primVal))
xx x = traverse (HR.transformTermHR (["Mod"])) (SexpParser.prettyParse x) 


hr :: (Show primTy, Show primVal,
 HasThrow
   "fromFrontendError"
   (Juvix.ToCore.Types.Error primTy primVal)
   (Feedback.FeedbackT [] P.String IO),
 HasReader
   "param"
   (Juvix.Core.Parameterisation.Parameterisation primTy primVal)
   (Feedback.FeedbackT [] P.String IO),
 HasState
   "coreSigs"
   (Juvix.ToCore.Types.CoreSigsHR primTy primVal)
   (Feedback.FeedbackT [] P.String IO)) =>
  ByteString
  -> IO (Either [Char] (Juvix.Core.HR.Types.Term primTy primVal))
hr x = do
  feedback <- Feedback.runFeedbackT $ xx x
  case feedback of
    Feedback.Success msgs c -> pure c
    Feedback.Fail msgs ->  panic $ show msgs

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
