module Main where

import Backends.LLVM (backendLLVM)
import Backends.Michelson (backendMichelson)
import Core.Common.Context (contextTests)
import Core.Common.NameSymb (top)
import Core.Conv (coreConversions)
import Core.EAC2 (eac2Tests)
import Core.Erasure (erasureTests)
import qualified Core.IR.Weak as Weak (top)
import Core.Parser (coreParser)
import Core.Typechecker (coreCheckerEval)
import Frontend.Desugar (allDesugar)
import Frontend.Golden (contractFiles)
import Frontend.Parser (allParserTests)
import FrontendContextualise.Infix.ShuntYard (allInfixTests)
import FrontendContextualise.Module.Open (openTests)
import Juvix.Library (IO)
import Pipeline (tests)
import qualified Test.Tasty as T

coreTests :: T.TestTree
coreTests =
  T.testGroup
    "Core tests"
    [ coreCheckerEval,
      coreConversions,
      coreParser
    ]

pipelineTests :: T.TestTree
pipelineTests =
  T.testGroup
    "Pipeline tests"
    tests

backendTests :: T.TestTree
backendTests =
  T.testGroup
    "Backend tests"
    [ -- ArithmeticCircuit.backendCircuit,
      backendLLVM,
      backendMichelson
    ]

frontEndTests :: T.TestTree
frontEndTests =
  T.testGroup
    "frontend tests"
    [allParserTests, contractFiles]

translationPasses :: T.TestTree
translationPasses =
  T.testGroup
    "translation passes from Frontend to Core"
    [allDesugar]

allCheckedTests :: T.TestTree
allCheckedTests =
  T.testGroup
    "All tests that are checked"
    [ coreTests,
      pipelineTests,
      backendTests,
      frontEndTests,
      translationPasses,
      eac2Tests,
      erasureTests,
      allInfixTests,
      contextTests,
      openTests,
      Weak.top,
      top
    ]

main :: IO ()
main = T.defaultMain allCheckedTests
