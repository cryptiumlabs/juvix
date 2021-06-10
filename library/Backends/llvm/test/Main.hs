module Main where

import Juvix.Library
import qualified Test.Golden as Golden
import qualified Test.Tasty as T

main :: IO ()
main = do
  goldenTests <- Golden.top
  T.defaultMain $
    T.testGroup "LLVM tests" [goldenTests]
