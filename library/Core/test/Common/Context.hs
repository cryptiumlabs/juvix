module Common.Context where

import Control.Lens (over)
import qualified Data.Text as Text
import qualified Juvix.Core.Common.Context as Context
import qualified Juvix.Core.Common.NameSpace as NameSpace
import Juvix.Library
import qualified Juvix.Library.HashMap as HashMap
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified StmContainers.Map as STM
import qualified System.IO.Unsafe as Unsafe
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

foo :: Context.T Int Int Int
foo = Unsafe.unsafePerformIO (Context.empty ("Foo" :| ["Bar", "Baz"]))

unsafeEmpty :: NameSymbol.T -> Context.T term ty sumRep
unsafeEmpty x = Unsafe.unsafePerformIO (Context.empty x)

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
  T.testCase
    "switch to module above and lookup value from below"
    ( do
        let added = Context.add (NameSpace.Pub "a") (Context.TypeDeclar 3) foo
            --
            looked = Context.lookup (pure "a") added
        Right switched <- Context.switchNameSpace ("Foo" :| ["Bar"]) added
        --
        let looked' = Context.lookup ("Baz" :| ["a"]) switched
        looked T.@=? looked'
    )

-- should we allow a switch to itself... should we just make it ID?
switchSelf :: T.TestTree
switchSelf =
  T.testCase
    "switching namespace to self is left"
    ( do
        swtiched <- Context.switchNameSpace ("Foo" :| ["Bar", "Baz"]) foo
        swtiched T.@=? Right foo
    )

checkFullyResolvedName :: T.TestTree
checkFullyResolvedName =
  T.testCase
    "relative lookup is the same as fully qualified"
    ( do
        Right relative <-
          Context.switchNameSpace (pure "Barry") foo
        --
        Right fullQual <-
          Context.switchNameSpace ("Foo" :| ["Bar", "Baz", "Barry"]) foo
        --
        Context.currentName relative T.@=? Context.currentName fullQual
    )

-- this test checks that the local variable is added and the global
checkCorrectResolution :: T.TestTree
checkCorrectResolution =
  T.testGroup
    "correct resolution test"
    [ T.testCase
        "topLevel value same as local: "
        ( do
            (outside, current) <- run
            Context.extractValue outside T.@=? Context.extractValue current
        ),
      T.testCase
        "topLevel is outside: "
        ( do
            (outside, _current) <- run
            isOutside outside T.@=? True
        ),
      T.testCase
        "current is local: "
        ( do
            (_, current) <- run
            isCurrent current T.@=? True
        )
    ]
  where
    isOutside (Context.Outside _) = True
    isOutside (Context.Current _) = False
    isCurrent (Context.Outside _) = False
    isCurrent (Context.Current _) = True
    run = do
      Right inner <- Context.switchNameSpace (pure "Gkar") foo
      --
      let added =
            Context.add (NameSpace.Pub "londo") (Context.TypeDeclar 3) inner
      --
      Right topGkar <-
        Context.switchNameSpace (Context.topLevelName :| ["Gkar"]) added
      --
      let addedTop =
            Context.add (NameSpace.Pub "londo") (Context.TypeDeclar 3) topGkar
      --
      Right switchBack <-
        Context.switchNameSpace ("Foo" :| ["Bar", "Baz"]) addedTop
      --
      let Just outside =
            switchBack Context.!? (Context.topLevelName :| ["Gkar", "londo"])
          --
          Just current = switchBack Context.!? ("Gkar" :| ["londo"])
      pure (outside, current)

privateFromAbove :: T.TestTree
privateFromAbove =
  T.testCase
    "Can't access private var from above"
    $ do
      empt <-
        Context.empty ("Ambassador" :| ["Kosh", "Vorlons"]) ::
          IO (Context.T Text.Text Text.Text Text.Text)
      --
      let added =
            Context.add
              (NameSpace.Priv "too-late")
              ( Context.TypeDeclar
                  "The avalanche has already started; It is too late for the pebbles to vote."
              )
              empt
      --
      Right switched <- Context.switchNameSpace ("Ambassador" :| ["Kosh"]) added
      --
      let looked = switched Context.!? ("Vorlons" :| ["too-late"])
      looked T.@=? Nothing

privateBeatsPublic :: T.TestTree
privateBeatsPublic =
  let empt :: Context.T Text.Text Text.Text Text.Text
      empt = unsafeEmpty ("Londo" :| ["Mollari", "Centauri"])
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
      empt = unsafeEmpty ("GKar" :| ["Narn"])
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
      looked = added2 Context.!? pure "cost"
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
  T.testCase
    "differnet module persists through switch"
    ( do
        Right topBar <- Context.switchNameSpace ("TopLevel" :| ["Bar"]) foo
        Right food <- Context.switchNameSpace ("TopLevel" :| ["Foo"]) topBar
        --
        let looked = food Context.!? (Context.topLevelName :| ["Bar"])
            --
            isOutSideRec (Just (Context.Outside (Context.Record {}))) = True
            isOutSideRec _ = False
        --
        isOutSideRec looked T.@=? True
    )

emptyWorksAsExpectedSingle :: T.TestTree
emptyWorksAsExpectedSingle =
  T.testCase
    "empty properly adds a top level module as expected:"
    $ do
      created <- Context.empty (pure "Mr-Morden") :: IO (Context.T Int Int Int)
      empt <- do
        contents <- atomically Context.emptyRecord
        pure $
          Context.T
            contents
            (pure "Mr-Morden")
            (HashMap.fromList [("Mr-Morden", Context.CurrentNameSpace)])
            HashMap.empty
      created T.@=? empt

topLevelDoesNotMessWithInnerRes :: T.TestTree
topLevelDoesNotMessWithInnerRes =
  T.testCase
    "TopLevelname does not prohbit inner module change"
    $ do
      let created :: Context.T Int Int Int
          created = unsafeEmpty (pure "Shadows")
      inner <-
        Context.switchNameSpace
          (Context.topLevelName :| ["Shadows", "Mr-Morden"])
          created
      inner2 <-
        Context.switchNameSpace
          ("Shadows" :| ["Mr-Morden"])
          created
      empt <- do
        emptyRecord <- atomically Context.emptyRecord
        emptyNameSpace <- atomically Context.emptyRecord
        emptyRecord
          |> over
            Context.contents
            (NameSpace.insert (NameSpace.Pub "Mr-Morden") Context.CurrentNameSpace)
          |> Context.Record
          |> (\record -> HashMap.fromList [("Shadows", record)])
          |> (\x -> Context.T emptyNameSpace ("Shadows" :| ["Mr-Morden"]) x HashMap.empty)
          |> pure
      inner == Right empt && inner == inner2 T.@=? True
