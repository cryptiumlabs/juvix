module Main where

import qualified Context
import qualified Test.Tasty as T

allCheckeddTests :: T.TestTree
allCheckeddTests =
  T.testGroup
    "Context tests"
    [Context.top]

main = T.defaultMain allCheckeddTests
