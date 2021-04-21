module Main where

import Juvix.Library
import qualified Test.Compiler as Compiler
import qualified Test.Groth as Groth
import qualified Test.Tasty as T

main :: IO ()
main =
  T.defaultMain $
    T.testGroup "Plonk tests" [Compiler.top, Groth.top]
