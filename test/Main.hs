module Main where

import qualified Backends.LLVM as LLVM
import qualified Backends.Michelson as Michelson
import qualified CoreConv
import qualified CoreParser
import qualified CoreTypechecker
-- import qualified Juvix.Frontend.Parser as Parser

import Data.Attoparsec.ByteString (IResult (..), Result, many1, parse)
import Data.ByteString (pack, unpack)
import Data.Text.Encoding (encodeUtf32BE)
import qualified EAC2
import qualified Erasure
import qualified Frontend
import qualified FrontendContextualise.Infix.ShuntYard as Shunt
import qualified FrontendDesugar
import qualified Juvix.Frontend.Lexer as Lexer
import Juvix.Frontend.Parser (removeComments, topLevelSN)
import Juvix.Frontend.Types (TopLevel)
import Juvix.Library hiding (identity)
import qualified Pipeline
import qualified Test.Tasty as T

coreTests :: T.TestTree
coreTests =
  T.testGroup
    "Core tests"
    [ CoreTypechecker.coreCheckerEval,
      CoreConv.coreConversions,
      CoreParser.coreParser
    ]

pipelineTests :: T.TestTree
pipelineTests =
  T.testGroup
    "Pipeline tests"
    Pipeline.tests

backendTests :: T.TestTree
backendTests =
  T.testGroup
    "Backend tests"
    [ -- ArithmeticCircuit.backendCircuit,
      LLVM.backendLLVM,
      Michelson.backendMichelson
    ]

frontEndTests :: T.TestTree
frontEndTests = T.testGroup "frontend tests" [Frontend.allParserTests]

allCheckedTests :: T.TestTree
allCheckedTests =
  T.testGroup
    "All tests that are checked"
    [ coreTests,
      pipelineTests,
      backendTests,
      frontEndTests,
      translationPasses,
      EAC2.eac2Tests,
      Erasure.erasureTests,
      Shunt.allInfixTests
    ]

translationPasses :: T.TestTree
translationPasses =
  T.testGroup
    "translation passes from Frontend to Core"
    [ FrontendDesugar.allDesugar
    ]

parseContract :: [Word8] -> Result [TopLevel]
parseContract raw =
  (parse (many1 topLevelSN) . removeComments) (pack (takeWhile (\x -> Lexer.space == x || Lexer.endOfLine x) raw))

contractTests :: FilePath -> IO ()
contractTests file = do
  parsed <- readFile file
  let rawContract = unpack $ encodeUtf32BE parsed
  case parseContract rawContract of
    Fail i context error ->
      putStrLn $ "Fail" <> i <> show context <> show error
    Done i r ->
      putStrLn $
        ("Success" :: ByteString)
          <> i
          <> show r
    Partial cont -> do
      putStrLn ("partial" :: ByteString)
      case (cont "") of
        Done i r ->
          putStrLn $
            ("Success (after partial) " :: ByteString)
              <> i
              <> show r
        Fail i context error ->
          putStrLn $ "Fail (after partial) " <> i <> show context <> show error
        Partial _cont' -> putStrLn ("Partial after Partial" :: ByteString)

main :: IO ()
main = do
  --T.defaultMain allCheckedTests
  contractTests "test/examples/Id-Strings.jvx"
-- contractTests "experimental/juvix/contract.jvx"
