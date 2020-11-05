module Main where

import Juvix.Library (IO)
import Parser (allParserTests)
import qualified Test.Tasty as T

allCheckedTests :: T.TestTree
allCheckedTests =
  T.testGroup
    "All tests that are checked"
    [allParserTests]

main :: IO ()
main = T.defaultMain allCheckedTests
