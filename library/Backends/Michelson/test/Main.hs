module Main where

import Juvix.Library 
import qualified Michelson
import qualified Interpreter
import qualified Test.Tasty as T
import qualified VStack

main = do
  end2End <- Interpreter.top
  T.defaultMain $ T.testGroup
    "All tests that are checked"
    [Michelson.top, VStack.top, end2End]
