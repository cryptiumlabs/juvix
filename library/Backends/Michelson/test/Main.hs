module Main where

import Juvix.Library (IO)
import qualified Test.Michelson as Michelson
import qualified Test.Pipeline as Pipeline
import qualified Test.Tasty as T
import qualified Test.VStack as VStack

allCheckedTests :: T.TestTree
allCheckedTests =
  T.testGroup
    "All tests that are checked"
    [Michelson.top, VStack.top, Pipeline.top]

main :: IO ()
main = T.defaultMain allCheckedTests
