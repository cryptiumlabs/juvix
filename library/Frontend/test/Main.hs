module Main where

import Juvix.Library (IO)
import Parser (allParserTests)
import qualified Sexp
import qualified Test.Tasty as T

allCheckedTests :: T.TestTree
allCheckedTests =
  T.testGroup
    "All tests that are checked"
    [allParserTests, Sexp.top]

main :: IO ()
main = T.defaultMain allCheckedTests
