{-# LANGUAGE LiberalTypeSynonyms #-}

module FrontendContextualise.Module.Open where

import qualified Juvix.Core.Common.Context as Context
import qualified Juvix.Core.Common.NameSpace as NameSpace
import qualified Juvix.FrontendContextualise.ModuleOpen.Environment as Env
import qualified Juvix.FrontendContextualise.ModuleOpen.Types as Types
import Juvix.Library
import qualified Juvix.Library.HashMap as Map
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

--------------------------------------------------------------------------------
-- Top test
--------------------------------------------------------------------------------

openTests :: T.TestTree
openTests =
  T.testGroup
    "Open Resolve Tests:"
    [openPreludeActsProperly]

--------------------------------------------------------------------------------
-- Setup Modules
--------------------------------------------------------------------------------

prelude :: Env.New Context.T
prelude =
  Context.empty (pure "Prelude")
    |> Context.add
      (NameSpace.Pub "->")
      ( Context.Def
          Nothing
          Nothing
          (Types.Like [] (Types.Primitive (Types.Prim (pure "Arrow"))) :| [])
          (Context.Pred Context.Right 0)
      )
    |> Context.add
      (NameSpace.Pub ":")
      ( Context.Def
          Nothing
          Nothing
          (Types.Like [] (Types.Primitive (Types.Prim (pure "Colon"))) :| [])
          (Context.Pred Context.Right 2)
      )

ourModule :: Env.New Context.T
ourModule =
  case Context.switchNameSpace (Context.topLevelName :| ["Londo"]) prelude of
    Right ctx -> ctx
    Left ____ -> prelude

firstOpen :: Either Env.Error Env.OpenMap
firstOpen =
  Env.resolve
    ourModule
    [ Env.Pre
        [Context.topLevelName :| ["Prelude"]]
        []
        (Context.topLevelName :| ["Londo"])
    ]

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

openPreludeActsProperly :: T.TestTree
openPreludeActsProperly =
  let Right opens = firstOpen
      (_, Env.Env {modMap}) =
        Env.bareRun
          Env.populateModMap
          (Context.empty (pure "Londo"))
          ourModule
          opens
      openMap =
        Map.fromList
          [ (":", Context.topLevelName :| ["Prelude"]),
            ("->", Context.topLevelName :| ["Prelude"])
          ]
   in T.testCase "-> and : should fully qualify" (modMap T.@=? openMap)
