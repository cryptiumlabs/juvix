module Main where

import Juvix.Library
import qualified Test.Context.Environment as Environment
import qualified Test.Contextify as Contextify
import qualified Test.Contextualise.Contextify as Contextualise
import Test.Contextualise.Infix.ShuntYard (allInfixTests)
import qualified Test.Desugar.Sexp as Sexp
import qualified Test.Golden as Golden
import qualified Test.Tasty as T

translationPasses :: T.TestTree
translationPasses =
  T.testGroup
    "translation passes from Frontend to Core"
    [Sexp.top, Environment.top, Contextify.top]

allCheckedTests :: T.TestTree
allCheckedTests =
  T.testGroup
    "All tests that are checked"
    [ allInfixTests,
      Contextualise.top,
      translationPasses
    ]

main :: IO ()
main = do
  p <- Golden.top
  T.defaultMain $
    T.testGroup
      "All tests"
      [ p,
        allCheckedTests
      ]
