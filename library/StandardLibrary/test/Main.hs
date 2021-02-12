module Main where

import Juvix.Library
import qualified NameSymb
import qualified Sexp
import qualified Test.Tasty as T

allCheckedTests :: T.TestTree
allCheckedTests =
  T.testGroup
    "All tests that are checked"
    [NameSymb.top, Sexp.top]

main :: IO ()
main = T.defaultMain allCheckedTests
