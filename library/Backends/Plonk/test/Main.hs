module Main where

import Juvix.Library
import qualified Test.Compiler as Compiler
import qualified Test.Tasty as T

main :: IO ()
main = do
  compilerTests <- Compiler.top
  T.defaultMain $
    T.testGroup "Plonk tests" [compilerTests]
