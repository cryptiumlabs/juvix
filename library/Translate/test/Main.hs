module Main where

import qualified Context.Environment
import qualified Contextify
import qualified Contextualise.Contextify
import Contextualise.Infix.ShuntYard (allInfixTests)
import qualified Desugar.Sexp as Sexp
import Golden (contractFiles)
import Juvix.Library (IO)
import qualified Test.Tasty as T

frontEndTests :: T.TestTree
frontEndTests =
  T.testGroup
    "frontend tests"
    [contractFiles]

translationPasses :: T.TestTree
translationPasses =
  T.testGroup
    "translation passes from Frontend to Core"
    [Sexp.top, Context.Environment.top, Contextify.top]

allCheckedTests :: T.TestTree
allCheckedTests =
  T.testGroup
    "All tests that are checked"
    [ frontEndTests,
      allInfixTests,
      Contextualise.Contextify.top,
      translationPasses
    ]

main :: IO ()
main = T.defaultMain allCheckedTests
