module Main where

import Juvix.Library (IO)
import Pipeline (tests)
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
    [pipelineTests]

main :: IO ()
main = T.defaultMain allCheckedTests
