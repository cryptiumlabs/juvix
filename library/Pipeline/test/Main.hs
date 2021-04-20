module Main where

import Juvix.Library (IO)
import Test.Pipeline (tests)
import qualified Test.RecGroups as RecGroups
import qualified Test.Tasty as T

pipelineTests :: T.TestTree
pipelineTests =
  T.testGroup
    "Pipeline tests"
    tests

allCheckedTests :: T.TestTree
allCheckedTests =
  T.testGroup
    "All tests that are checked"
    [pipelineTests, RecGroups.top]

main :: IO ()
main = T.defaultMain allCheckedTests
