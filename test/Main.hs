module Main where

import qualified Backends.ArithmeticCircuit as ArithmeticCircuit
import qualified Backends.LLVM as LLVM
import qualified Backends.Michelson as Michelson
import qualified CoreConv
import qualified CoreParser
import qualified CoreTypechecker
import Data.Text.Encoding (encodeUtf32BE)
import qualified EAC2
import qualified Erasure
import qualified Frontend
import qualified FrontendContextualise.Infix.ShuntYard as Shunt
import qualified FrontendDesugar
import qualified Juvix.Frontend.Parser as Parser
import Juvix.Library hiding (identity)
import qualified Pipeline
import qualified Test.Tasty as T

-- import qualified Test.Tasty.QuickCheck as T

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

contractTests :: FilePath -> IO ()
contractTests file = do
  parsed <- readFile $ file
  let parsedIdString = Parser.parse (encodeUtf32BE parsed)
  case parsedIdString of
    Left s -> putStrLn s
    Right _t -> putStrLn $ "parsed " <> file

-- putStr (show t) TODO make it show t

main :: IO ()
main = do
  T.defaultMain allCheckedTests
  contractTests "test/examples/Id-Strings.jvx"
  contractTests "experimental/juvix/contract.jvx"
