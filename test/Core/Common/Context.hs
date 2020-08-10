module Core.Common.Context where

import qualified Juvix.Core.Common.Context as Context
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
