module Main where

import qualified Backends.LLVM as LLVM
import qualified Backends.Michelson as Michelson
import qualified Core.Common.Context as Context
import qualified Core.Common.NameSymb as NameSymb
import qualified Core.Conv as Conv
import qualified Core.EAC2 as EAC2
import qualified Core.Erasure as Erasure
import qualified Core.IR.Weak as Weak
import qualified Core.Parser as Parser
import qualified Core.Typechecker as Typechecker
import qualified Frontend.Desugar as Desugar
import qualified Frontend.Parser as Parser
import qualified FrontendContextualise.Infix.ShuntYard as Shunt
import qualified FrontendContextualise.Module.Open as Open
import Juvix.Library hiding (identity)
import qualified Pipeline
import qualified Test.Tasty as T

coreTests :: T.TestTree
coreTests =
  T.testGroup
    "Core tests"
    [ Typechecker.coreCheckerEval,
      Conv.coreConversions,
      Parser.coreParser
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
frontEndTests = T.testGroup "frontend tests" [Parser.allParserTests]

translationPasses :: T.TestTree
translationPasses =
  T.testGroup
    "translation passes from Frontend to Core"
    [ Desugar.allDesugar
    ]

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
      -- Shunt.allInfixTests,
      Context.contextTests,
      Open.openTests,
      Weak.top,
      NameSymb.top
    ]

main :: IO ()
main = T.defaultMain allCheckedTests
