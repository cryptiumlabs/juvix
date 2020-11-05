module Main where

import Juvix.Library (IO)
import qualified Test.Tasty as T

allCheckedTests :: T.TestTree
allCheckedTests =
  T.testGroup
    "All tests that are checked"
    []

-- ArithmeticCircuit.backendCircuit

main :: IO ()
main = T.defaultMain allCheckedTests
