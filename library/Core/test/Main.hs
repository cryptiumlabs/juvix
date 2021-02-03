module Main where

import Common.Context (contextTests)
import Conv (coreConversions)
import Erasure (erasureTests)
import Evaluator (evaluatorTests)
import qualified IR.Weak as Weak
import Juvix.Library (IO)
import Parser (coreParser)
import qualified Test.Tasty as T
import Typechecker (typecheckerTests)

coreTests :: T.TestTree
coreTests =
  T.testGroup
    "Core tests"
    [ typecheckerTests,
      evaluatorTests,
      coreConversions,
      coreParser
    ]

allCheckedTests :: T.TestTree
allCheckedTests =
  T.testGroup
    "All tests that are checked"
    [coreTests, erasureTests, contextTests, Weak.top]

main :: IO ()
main = T.defaultMain allCheckedTests
