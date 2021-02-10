{-# LANGUAGE LiberalTypeSynonyms #-}

module Contextualise.Module.Resolve where

import qualified Juvix.Core.Common.Context as Context
import qualified Juvix.Core.Common.NameSpace as NameSpace
import qualified Juvix.FrontendContextualise.Contextify.ResolveOpenInfo as Resolve
import qualified Juvix.FrontendContextualise.ModuleOpen.Environment as Env
import qualified Juvix.FrontendContextualise.ModuleOpen.Types as Types
import Juvix.Library
import qualified Juvix.Library.HashMap as Map
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T
import qualified Contextualise.Module.Open as Open


firstResolve = do
  our <- Open.ourModule
  Resolve.run
    our
    [Resolve.Pre
       [Context.topLevelName :| ["Prelude"]]
       []
       (Context.topLevelName :| ["Londo"])
    ]
