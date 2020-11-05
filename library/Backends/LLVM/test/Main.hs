module Main where

import Juvix.Library (IO)
import LLVM (backendLLVM)
import qualified Test.Tasty as T

allCheckedTests :: T.TestTree
allCheckedTests =
  T.testGroup
    "All tests that are checked"
    [backendLLVM]

main :: IO ()
main = T.defaultMain allCheckedTests
