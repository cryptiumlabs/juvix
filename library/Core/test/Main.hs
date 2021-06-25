module Main where

import Conv (coreConversions)
import Erasure (erasureTests)
import qualified HR.Pretty
import qualified IR.Weak as Weak
import Juvix.Library (IO)
import qualified Test.Tasty as T
import Typechecker (coreCheckerEval)
import qualified Utility

coreTests :: T.TestTree
coreTests =
  T.testGroup
    "Core tests"
    [ coreCheckerEval,
      coreConversions
    ]

allCheckedTests :: T.TestTree
allCheckedTests =
  T.testGroup
    "All tests that are checked"
    [ coreTests,
      erasureTests,
      Weak.top,
      HR.Pretty.top,
      Utility.top
    ]

main :: IO ()
main = T.defaultMain allCheckedTests
