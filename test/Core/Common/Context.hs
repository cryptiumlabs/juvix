module Core.Common.Context where

import qualified Juvix.Core.Common.Context as Context
import Juvix.Library
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

memptyTest :: Context.Definition Natural Natural
memptyTest =
  foldr const (Context.Def mempty 1 1) (fmap identity (Context.fromList []))
