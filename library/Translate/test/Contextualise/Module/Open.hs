{-# LANGUAGE LiberalTypeSynonyms #-}

module Contextualise.Module.Open where

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
    [ openPreludeActsProperly,
      topLevelToImportDoesntMatter,
      ambiSymbolInOpen,
      notAmbiIfLocalF,
      notAmbiIfTopLevel,
      onlyOpenProperSymbols
    ]

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

resolveOurModule :: Either Env.Error Env.OpenMap
resolveOurModule =
  Env.resolve
    ourModule
    [ Env.Pre
        [Context.topLevelName :| ["Prelude"]]
        []
        (Context.topLevelName :| ["Londo"])
    ]

sameSymbolModule :: Env.New Context.T
sameSymbolModule =
  let added =
        Context.add (NameSpace.Pub "phantasm") (defaultDef "phantasm") ourModule
      Right switched =
        Context.switchNameSpace (Context.topLevelName :| ["Stirner"]) added
      added2 =
        Context.add (NameSpace.Pub "phantasm") (defaultDef "of-the-mind") switched
   in added2

defaultDef :: Symbol -> Env.New Context.Definition
defaultDef symb =
  Context.Def
    Nothing
    Nothing
    (Types.Like [] (Types.Primitive (Types.Prim (pure symb))) :| [])
    (Context.Pred Context.Right 10)

preludeAdded :: Env.New Context.T
preludeAdded =
  let added =
        Context.add (NameSpace.Pub "Prelude") (defaultDef "") sameSymbolModule
      Right switched =
        Context.switchNameSpace (Context.topLevelName :| ["Londo"]) added
      added' =
        Context.add (NameSpace.Pub "Prelude") (defaultDef "") switched
      Right switched' =
        Context.switchNameSpace (Context.topLevelName :| ["Max"]) added'
      added'' =
        Context.add (NameSpace.Pub "phantasm") (defaultDef "") switched'
   in added''

resolvePreludeAdded :: Either Env.Error Env.OpenMap
resolvePreludeAdded =
  [Env.Pre [pure "Stirner", pure "Londo"] [] (Context.topLevelName :| ["Max"])]
    |> Env.resolve preludeAdded

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

topLevelToImportDoesntMatter :: T.TestTree
topLevelToImportDoesntMatter =
  Env.resolve
    ourModule
    [ Env.Pre
        [pure "Prelude"]
        []
        (Context.topLevelName :| ["Londo"])
    ]
    |> (resolveOurModule T.@=?)
    |> T.testCase
      "adding top level to module open does not matter if there is no lower module"

openPreludeActsProperly :: T.TestTree
openPreludeActsProperly =
  let Right opens = resolveOurModule
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

ambiSymbolInOpen :: T.TestTree
ambiSymbolInOpen =
  let Right switched =
        Context.switchNameSpace (Context.topLevelName :| ["Max"]) sameSymbolModule
   in [Env.Pre [pure "Stirner", pure "Londo"] [] (Context.topLevelName :| ["Max"])]
        |> Env.resolve switched
        |> (T.@=? Left (Env.AmbiguousSymbol "phantasm"))
        |> T.testCase
          "opening same symbol with it not being in the module def should be ambi"

notAmbiIfLocalF :: T.TestTree
notAmbiIfLocalF =
  let Right switched =
        Context.switchNameSpace (Context.topLevelName :| ["Max"]) sameSymbolModule
      added = Context.add (NameSpace.Pub "phantasm") (defaultDef "") switched
      opens =
        Map.fromList
          [ ( Context.topLevelName :| ["Max"],
              [ Env.Explicit (Context.topLevelName :| ["Stirner"]),
                Env.Explicit (Context.topLevelName :| ["Londo"])
              ]
            )
          ]
   in [Env.Pre [pure "Stirner", pure "Londo"] [] (Context.topLevelName :| ["Max"])]
        |> Env.resolve added
        |> (T.@=? Right opens)
        |> T.testCase
          "opening same symbol with it being in the module is not ambi"

notAmbiIfTopLevel :: T.TestTree
notAmbiIfTopLevel =
  let opens =
        Map.fromList
          [ ( Context.topLevelName :| ["Max"],
              [ Env.Explicit (Context.topLevelName :| ["Stirner"]),
                Env.Explicit (Context.topLevelName :| ["Londo"])
              ]
            )
          ]
   in resolvePreludeAdded
        |> (T.@=? Right opens)
        |> T.testCase
          "opening same symbol with it being in the module is not ambi"

onlyOpenProperSymbols :: T.TestTree
onlyOpenProperSymbols =
  let Right opens = resolvePreludeAdded
      (_, Env.Env {modMap}) =
        Env.bareRun
          Env.populateModMap
          (Context.empty (pure "Max"))
          preludeAdded
          opens
   in (modMap T.@=? mempty)
        |> T.testCase
          "explicit symbols don't get added to the symbol mapping"
