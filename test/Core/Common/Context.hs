module Core.Common.Context where

import qualified Juvix.Core.Common.Context as Context
import qualified Juvix.Core.Common.NameSpace as NameSpace
import Juvix.Library
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

-- nestedRecord :: Context.T term ty sumRep
-- nestedRecord =
--   Context.fromList
--     [ ( "foo",
--         Context.Record
--           (Context.fromList [("a", Context.Record (Context.T mempty) Nothing)])
--           Nothing
--       )
--     ]

-- memptyTest :: Context.Definition Natural Natural Natural
-- memptyTest =
--   foldr
--     const
--     (Context.Def Nothing Nothing 1 Context.default')
--     (fmap identity (Context.fromList []))

-- lookupTest :: Maybe (Context.Definition term ty sumRep)
-- lookupTest = Context.lookup "foo.a" nestedRecord

foo :: Context.T Int Int Int
foo = Context.empty ("Foo" :| ["Bar", "Baz"])

foo' =
  Context.addPathWithValue
    ("Foo" :| ["Bar", "Baz", "Barry"])
    (Context.Unknown Nothing)
    foo

foo'' = Context.switchNameSpace ("Foo" :| ["Bar", "Baz", "Barry"]) foo

foo''' = Context.switchNameSpace ("Foo" :| ["Bar", "Min"]) foo

foo'''' = Context.switchNameSpace ("Foo" :| ["Bar", "Baz"]) foo

barTest =
  let Right foo = foo'
   in Context.switchNameSpace ("Foo" :| ["Bar", "Baz", "Barr"]) foo

barTest' =
  let Right bar = barTest
   in Context.removeNameSpace ("Foo" :| ["Bar", "Baz", "Barry"]) bar

testInner =
  let Right foo = foo'
   in Context.switchNameSpace ("TopLevel" :| ["Foo"]) foo

switchTest =
  let Right foo = foo'
      Right foo'' = Context.switchNameSpace ("TopLevel" :| ["Bar"]) foo
   in Context.switchNameSpace ("TopLevel" :| ["Foo"]) foo''

contextTests :: T.TestTree
contextTests =
  T.testGroup
    "Context tests:"
    [ switchAboveLookupCheck,
      switchSelf,
      checkFullyResolvedName,
      checkCorrectResolution
    ]

switchAboveLookupCheck :: T.TestTree
switchAboveLookupCheck =
  let added = Context.add (NameSpace.Pub "a") (Context.TypeDeclar 3) foo
      looked = Context.lookup (pure "a") added
      Right switched = Context.switchNameSpace ("Foo" :| ["Bar"]) added
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
        T.@=? Left (Context.VariableShared ("Foo" :| ["Bar", "Baz"]))
    )

checkFullyResolvedName :: T.TestTree
checkFullyResolvedName =
  let Right relative = Context.switchNameSpace (pure "Barry") foo
      Right fullQual = Context.switchNameSpace ("Foo" :| ["Bar", "Baz", "Barry"]) foo
   in T.testCase
        "relative lookup is the same as fully qualified"
        (Context.currentName relative T.@=? Context.currentName fullQual)

-- this test checks that the local variable is added and the global
checkCorrectResolution :: T.TestTree
checkCorrectResolution =
  let Right inner = Context.switchNameSpace (pure "Gkar") foo
      added = Context.add (NameSpace.Pub "londo") (Context.TypeDeclar 3) inner
      Right topGkar = Context.switchNameSpace (Context.topLevelName :| ["Gkar"]) added
      addedTop = Context.add (NameSpace.Pub "londo") (Context.TypeDeclar 3) topGkar
      Right switchBack = Context.switchNameSpace ("Foo" :| ["Bar", "Baz"]) addedTop
      Just outside = switchBack Context.!? (Context.topLevelName :| ["Gkar", "londo"])
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
            (isCurrent current T.@=? False)
        ]
  where
    isOutside (Context.Outside _) = True
    isOutside (Context.Current _) = False
    isCurrent (Context.Outside _) = False
    isCurrent (Context.Current _) = True
