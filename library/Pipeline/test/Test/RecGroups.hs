{-# LANGUAGE LiberalTypeSynonyms #-}

module Test.RecGroups where

import qualified Juvix.Core.Common.Context.Traverse as Traverse
import Juvix.Library
import qualified Juvix.Pipeline as Pipeline
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

juvixRootPath :: FilePath 
juvixRootPath = "../../"

withJuvixRootPath :: FilePath -> FilePath
withJuvixRootPath p = juvixRootPath <> p

withJuvixExamplesPath :: FilePath -> FilePath
withJuvixExamplesPath p = juvixRootPath <> "test/examples/" <> p


top :: T.TestTree
top =
  T.testGroup
    "Rec Groups tests"
    [pipeline, pipelineOpen]

pipeline :: T.TestTree
pipeline =
  let correctOrder =
        [ "Rec-Groups-Helper" :| ["ty_"],
          "Rec-Groups-Helper" :| ["ty"],
          "Rec-Groups-Helper" :| ["foo"],
          "Rec-Groups" :| ["main"]
        ]
   in T.testCase
        "multiple modules have correct ordering"
        $ do
          Right c <-
            Pipeline.toCore
              (withJuvixExamplesPath <$> [ 
                "positive/michelson/rec-groups/Rec-Groups.ju",
                "positive/michelson/rec-groups/Rec-Groups-Helper.ju"
              ])
          let recd = Traverse.recGroups c
          fmap (\(x :| []) -> Traverse.name x) recd T.@=? correctOrder

pipelineOpen :: T.TestTree
pipelineOpen =
  let correctOrder =
        [ "Foo-Helper" :| ["bar"],
          "Foo-Helper" :| ["Bah", "fi"],
          -- DUPLICATE!?
          "Foo-Helper" :| ["Bah", "fi"],
          "Foo-Helper" :| ["Bah", "Baz", "si"],
          "Identity" :| ["fi"],
          "Identity" :| ["main"]
        ]
   in T.testCase
        "multiple modules have correct ordering"
        $ do
          Right c <-
            Pipeline.toCore
              (withJuvixExamplesPath <$> [ 
                "positive/michelson/test/Foo.ju",
                "positive/michelson/test/Foo-Helper.ju",
                "positive/michelson/test/Bah.ju",
                "positive/michelson/test/Baz.ju"
              ])
          let recd = Traverse.recGroups c
          fmap (\(x :| []) -> Traverse.name x) recd T.@=? correctOrder
