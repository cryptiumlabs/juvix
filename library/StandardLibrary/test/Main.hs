module Main where

import Juvix.Library
import qualified Juvix.Library.Sexp.Types (Atom, lengthM) -- we may not use it, but it makes liquid happy
import qualified NameSymb
import qualified Sexp
import qualified Sexp.Parser
import qualified Sexp.SimplifiedPasses
import qualified Test.Tasty as T

allCheckedTests :: T.TestTree
allCheckedTests =
  T.testGroup
    "All tests that are checked"
    [NameSymb.top, Sexp.top, Sexp.Parser.top, Sexp.SimplifiedPasses.top]

main :: IO ()
main = T.defaultMain allCheckedTests
