module Utility (top) where

import qualified Data.IntSet as PS
import qualified Juvix.Core.Utility as Util
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

top :: T.TestTree
top =
  T.testGroup
    "Utility tests"
    [patVarsTests]

patVarsTests :: T.TestTree
patVarsTests =
  T.testGroup
    "pattern variable tests:"
    [ T.testCase
        "generates from 0"
        (Util.take 10 (Util.patVarsExcept (PS.fromList [])) T.@=? [0 .. 9]),
      T.testCase
        "patVar properly excludes given elements"
        (Util.take 10 (Util.patVarsExcept (PS.fromList [0, 2 .. 18])) T.@=? [1, 3 .. 19])
    ]
