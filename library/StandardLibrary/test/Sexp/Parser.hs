{-# LANGUAGE NamedFieldPuns #-}

module Sexp.Parser where

import Juvix.Library
import qualified Juvix.Library.Sexp as Sexp
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

top :: T.TestTree
top =
  T.testGroup
    "sexp parser tests"
    [ staringParenSpaceDoesntEffectParser,
      endingParenSpaceDoesntEffectParser,
      uncidoeParserCorrectly,
      numbersParseCorrectly
    ]

staringParenSpaceDoesntEffectParser :: T.TestTree
staringParenSpaceDoesntEffectParser =
  T.testCase
    "space after () works as expected"
    (Sexp.parse "(1 2 3)" T.@=? Sexp.parse " (       1 2 3)")

endingParenSpaceDoesntEffectParser :: T.TestTree
endingParenSpaceDoesntEffectParser =
  T.testCase
    "space after () works as expected"
    (Sexp.parse "(1 2 3)" T.@=? Sexp.parse " (1 2 3              )       ")

uncidoeParserCorrectly :: T.TestTree
uncidoeParserCorrectly =
  T.testCase
    "space after () works as expected"
    ("Foo" :| ["X:Y<Z", "A---B-C"] T.@=? atomName)
  where
    Right (Sexp.Atom Sexp.A {Sexp.atomName}) = Sexp.parse "Foo.X:Y<Z.A---B-C"

numbersParseCorrectly :: T.TestTree
numbersParseCorrectly =
  T.testCase
    "Numbers parse correctly"
    (Right (Sexp.Atom (Sexp.N 3 Nothing)) T.@=? Sexp.parse "3")
