module Main where

import Juvix.Library (IO)
import qualified Test.Michelson as Michelson
import qualified Test.Pipeline as Pipeline
import qualified Test.Tasty as T
import qualified Test.VStack as VStack
import qualified Test.Golden as Golden

allCheckedTests :: IO T.TestTree
allCheckedTests = do
  goldenTests <- Golden.top
  T.testGroup
    "All tests that are checked"
    [Michelson.top, VStack.top, Pipeline.top, goldenTests]

main :: IO ()
main = allCheckedTests >>= T.defaultMain 
