module Main where

import Juvix.Library (IO)
import qualified Michelson
import qualified Test.Tasty as T
import qualified VStack

allCheckedTests :: T.TestTree
allCheckedTests =
  T.testGroup
    "All tests that are checked"
    [Michelson.top, VStack.top]

main :: IO ()
main = T.defaultMain allCheckedTests
