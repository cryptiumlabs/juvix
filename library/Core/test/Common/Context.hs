module Common.Context where

import qualified Data.Text as Text
import qualified Juvix.Core.Common.Context as Context
import qualified Juvix.Core.Common.NameSpace as NameSpace
import Juvix.Library
import qualified Juvix.Library.HashMap as HashMap
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

foo :: Context.T Int Int Int
foo = Context.empty ("Foo" :| ["Bar", "Baz"])

contextTests :: T.TestTree
contextTests =
  T.testGroup
    "Context tests:"
    [ switchAboveLookupCheck,
      switchSelf,
      checkFullyResolvedName,
      checkCorrectResolution,
      privateFromAbove,
      privateBeatsPublic,
      localBeatsGlobal,
      nonRelatedModuleStillPersists,
      emptyWorksAsExpectedSingle,
      topLevelDoesNotMessWithInnerRes
    ]

switchAboveLookupCheck :: T.TestTree
switchAboveLookupCheck =
  let added = Context.add (NameSpace.Pub "a") (Context.TypeDeclar 3) foo
      --
      looked = Context.lookup (pure "a") added
      --
      Right switched = Context.switchNameSpace ("Foo" :| ["Bar"]) added
      --
      looked' = Context.lookup ("Baz" :| ["a"]) switched
   in T.testCase
        "switch to module above and lookup value from below"
        (looked T.@=? looked')

-- should we allow a switch to itself... should we just make it ID?
switchSelf :: T.TestTree
switchSelf =
  T.testCase
    "switching namespace to self is left"
    ( Context.switchNameSpace ("Foo" :| ["Bar", "Baz"]) foo
        T.@=? Right foo
    )

checkFullyResolvedName :: T.TestTree
checkFullyResolvedName =
  let Right relative =
        Context.switchNameSpace (pure "Barry") foo
      --
      Right fullQual =
        Context.switchNameSpace ("Foo" :| ["Bar", "Baz", "Barry"]) foo
   in T.testCase
        "relative lookup is the same as fully qualified"
        (Context.currentName relative T.@=? Context.currentName fullQual)

-- this test checks that the local variable is added and the global
checkCorrectResolution :: T.TestTree
checkCorrectResolution =
  let Right inner = Context.switchNameSpace (pure "Gkar") foo
      --
      added =
        Context.add (NameSpace.Pub "londo") (Context.TypeDeclar 3) inner
      --
      Right topGkar =
        Context.switchNameSpace (Context.topLevelName :| ["Gkar"]) added
      --
      addedTop =
        Context.add (NameSpace.Pub "londo") (Context.TypeDeclar 3) topGkar
      --
      Right switchBack =
        Context.switchNameSpace ("Foo" :| ["Bar", "Baz"]) addedTop
      --
      Just outside =
        switchBack Context.!? (Context.topLevelName :| ["Gkar", "londo"])
      --
      Just current = switchBack Context.!? ("Gkar" :| ["londo"])
   in T.testGroup
        "correct resolution test"
        [ T.testCase
            "topLevel value same as local: "
            (Context.extractValue outside T.@=? Context.extractValue current),
          T.testCase
            "topLevel is outside: "
            (isOutside outside T.@=? True),
          T.testCase
            "current is local: "
            (isCurrent current T.@=? True)
        ]
  where
    isOutside (Context.Outside _) = True
    isOutside (Context.Current _) = False
    isCurrent (Context.Outside _) = False
    isCurrent (Context.Current _) = True

privateFromAbove :: T.TestTree
privateFromAbove =
  let empt :: Context.T Text.Text Text.Text Text.Text
      empt = Context.empty ("Ambassador" :| ["Kosh", "Vorlons"])
      --
      added =
        Context.add
          (NameSpace.Priv "too-late")
          ( Context.TypeDeclar
              "The avalanche has already started; It is too late for the pebbles to vote."
          )
          empt
      Right switched =
        Context.switchNameSpace ("Ambassador" :| ["Kosh"]) added
      looked = switched Context.!? ("Vorlons" :| ["too-late"])
   in T.testCase
        "Can't access private var from above"
        (looked T.@=? Nothing)

privateBeatsPublic :: T.TestTree
privateBeatsPublic =
  let empt :: Context.T Text.Text Text.Text Text.Text
      empt = Context.empty ("Londo" :| ["Mollari", "Centauri"])
      --
      added =
        Context.add
          (NameSpace.Priv "joy")
          ( Context.TypeDeclar
              "What do you want, you moon-faced assassin of joy?"
          )
          empt
      added2 =
        Context.add
          (NameSpace.Pub "joy")
          ( Context.TypeDeclar
              "Now, I go to spread happiness to the rest of the station. \
              \ It is a terrible responsibility but I have learned to live with it."
          )
          added
      looked = added2 Context.!? pure "joy"
   in "What do you want, you moon-faced assassin of joy?"
        |> Context.TypeDeclar
        |> NameSpace.Priv
        |> Context.Current
        |> Just
        |> (looked T.@=?)
        |> T.testCase "Can't access private var from above"

localBeatsGlobal :: T.TestTree
localBeatsGlobal =
  let empt :: Context.T Text.Text Text.Text Text.Text
      empt = Context.empty ("GKar" :| ["Narn"])
      --
      added =
        Context.add
          (NameSpace.Priv "cost")
          ( Context.TypeDeclar
              "I have seen what power does, and I have seen what power costs. \
              \ The one is never equal to the other."
          )
          empt
      added2 =
        Context.addGlobal
          (Context.topLevelName :| ["cost"])
          ( Context.TypeDeclar
              "I'm delirious with joy. It proves that if you confront the universe \
              \ with good intentions in your heart, it will reflect that and reward \
              \ your intent. Usually. It just doesn't always do it in the way you expect."
          )
          added
      looked = added2 Context.!? (pure "cost")
   in "I have seen what power does, and I have seen what power costs. \
      \ The one is never equal to the other."
        |> Context.TypeDeclar
        |> NameSpace.Priv
        |> Context.Current
        |> Just
        |> (looked T.@=?)
        |> T.testCase "public beats global"

nonRelatedModuleStillPersists :: T.TestTree
nonRelatedModuleStillPersists =
  let Right topBar = Context.switchNameSpace ("TopLevel" :| ["Bar"]) foo
      Right food = Context.switchNameSpace ("TopLevel" :| ["Foo"]) topBar
      --
      looked = food Context.!? (Context.topLevelName :| ["Bar"])
      --
      isOutSideRec (Just (Context.Outside (Context.Record _ _))) = True
      isOutSideRec _ = False
   in T.testCase
        "differnet module persists through switch"
        (isOutSideRec looked T.@=? True)

emptyWorksAsExpectedSingle :: T.TestTree
emptyWorksAsExpectedSingle =
  let created :: Context.T Int Int Int
      created = Context.empty (pure "Mr-Morden")
      empt =
        HashMap.fromList [("Mr-Morden", Context.CurrentNameSpace)]
          |> Context.T NameSpace.empty (pure "Mr-Morden")
   in T.testCase
        "empty properly adds a top level module as expected:"
        (created T.@=? empt)

topLevelDoesNotMessWithInnerRes :: T.TestTree
topLevelDoesNotMessWithInnerRes =
  let created :: Context.T Int Int Int
      created = Context.empty (pure "Shadows")
      inner =
        Context.switchNameSpace
          (Context.topLevelName :| ["Shadows", "Mr-Morden"])
          created
      inner2 =
        Context.switchNameSpace
          ("Shadows" :| ["Mr-Morden"])
          created
      empt =
        NameSpace.empty
          |> NameSpace.insert (NameSpace.Pub "Mr-Morden") Context.CurrentNameSpace
          |> flip Context.Record Nothing
          |> (\record -> HashMap.fromList [("Shadows", record)])
          |> Context.T NameSpace.empty ("Shadows" :| ["Mr-Morden"])
   in T.testCase
        "TopLevelname does not prohbit inner module change"
        (inner == Right empt && inner == inner2 T.@=? True)
