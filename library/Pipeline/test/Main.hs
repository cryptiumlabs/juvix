module Main where

import Juvix.Library (IO)
import qualified Test.RecGroups as RecGroups
import qualified Test.Tasty as T

allCheckedTests :: T.TestTree
allCheckedTests =
  T.testGroup
    "All tests that are checked"
    [RecGroups.top]

main :: IO ()
main = T.defaultMain allCheckedTests
