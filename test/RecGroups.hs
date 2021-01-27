{-# LANGUAGE LiberalTypeSynonyms #-}

module RecGroups where

import qualified Juvix.Core.Common.Context.Traverse as Traverse
import Juvix.Library
import qualified Juvix.Pipeline as Pipeline
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

top :: T.TestTree
top =
  T.testGroup
    "Rec Groups tests"
    [pipeline]

pipeline :: T.TestTree
pipeline =
  let correctOrder =
        ["Rec-Groups-Helper" :| ["ty_"], "Rec-Groups-Helper" :| ["ty"], "Rec-Groups-Helper" :| ["foo"], "Rec-Groups" :| ["main"]]
   in T.testCase
        "multiple modules have correct ordering"
        $ do
          Right c <- Pipeline.toCore ["test/examples/rec-groups/rec-groups.ju", "test/examples/rec-groups/rec-groups-helper.ju"]
          let recd = Traverse.recGroups c
          fmap (\(x :| []) -> Traverse.name x) recd T.@=? correctOrder
