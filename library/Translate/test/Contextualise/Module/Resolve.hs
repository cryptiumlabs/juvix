{-# LANGUAGE LiberalTypeSynonyms #-}

module Contextualise.Module.Resolve where

import qualified Contextualise.Module.Open as Open
import Control.Lens hiding ((|>))
import qualified Juvix.Core.Common.Context as Context
import qualified Juvix.Core.Common.Context as Ctx
import qualified Juvix.Core.Common.NameSpace as NameSpace
import qualified Juvix.FrontendContextualise.Contextify.ResolveOpenInfo as Resolve
import qualified Juvix.FrontendContextualise.ModuleOpen.Environment as Env
import qualified Juvix.FrontendContextualise.ModuleOpen.Types as Types
import Juvix.Library
import qualified Juvix.Library.HashMap as Map
import qualified ListT
import qualified StmContainers.Map as STM
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

top :: T.TestTree
top =
  T.testGroup
    "context resolve tests:"
    [ preludeProperlyOpens
    ]

firstResolve :: IO (Either Resolve.Error (Env.New Context.T))
firstResolve = do
  our <- Open.ourModule
  Resolve.run
    our
    [ Resolve.Pre
        [Context.topLevelName :| ["Prelude"]]
        []
        (Context.topLevelName :| ["Londo"])
    ]

preludeProperlyOpens :: T.TestTree
preludeProperlyOpens =
  T.testCase
    "-> and : should be in the qualifying map"
    ( do
        Right resolved <- firstResolve
        let stmMap = resolved ^. Context._currentNameSpace . Context.qualifiedMap
            answer =
              [ (":", Ctx.SymInfo {used = Ctx.NotUsed, mod = "TopLevel" :| ["Prelude"]}),
                ("->", Ctx.SymInfo {used = Ctx.NotUsed, mod = "TopLevel" :| ["Prelude"]})
              ]
        reality <- atomically (ListT.toList $ STM.listT stmMap)
        reality T.@=? answer
    )
