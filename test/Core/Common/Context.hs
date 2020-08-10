module Core.Common.Context where

import qualified Juvix.Core.Common.Context as Context
import Juvix.Library
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

nestedRecord :: Context.T term ty sumRep
nestedRecord =
  Context.fromList
    [ ( "foo",
        Context.Record
          (Context.fromList [("a", Context.Record (Context.T mempty) Nothing)])
          Nothing
      )
    ]

memptyTest :: Context.Definition Natural Natural Natural
memptyTest =
  foldr
    const
    (Context.Def Nothing Nothing 1 Context.default')
    (fmap identity (Context.fromList []))

lookupTest :: Maybe (Context.Definition term ty sumRep)
lookupTest = Context.lookup "foo.a" nestedRecord

foo = Juvix.Core.Common.Context.empty ("Foo" :| ["Bar", "Baz"])

foo' = addPathWithValue ("Foo" :| ["Bar", "Baz", "Barry"]) (Unknown Nothing) foo

foo'' = switchNameSpace ("Foo" :| ["Bar", "Baz", "Barry"]) foo

foo''' = switchNameSpace ("Foo" :| ["Bar", "Min"]) foo

foo'''' = switchNameSpace ("Foo" :| ["Bar", "Baz"]) foo

barTest = switchNameSpace ("Foo" :| ["Bar", "Baz", "Barr"]) foo'

barTest' = removeNameSpace ("Foo" :| ["Bar", "Baz", "Barry"]) barTest

testInner = switchNameSpace ("TopLevel" :| ["Foo"]) foo'

switchTest =
  let Right foo'' = switchNameSpace ("TopLevel" :| ["Bar"]) foo'
   in switchNameSpace ("TopLevel" :| ["Foo"]) foo''
