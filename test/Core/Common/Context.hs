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
  foldr const (Context.Def mempty 1 1) (fmap identity (Context.fromList []))

lookupTest :: Maybe (Context.Definition term ty sumRep)
lookupTest = Context.lookup "foo.a" nestedRecord
