{-# LANGUAGE LiberalTypeSynonyms #-}

module Contextualise.Module.Open where

import qualified Juvix.Core.Common.Context as Context
import qualified Juvix.Core.Common.NameSpace as NameSpace
import qualified Juvix.FrontendContextualise.Contextify.ResolveOpenInfo as Resolve
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
    [ -- openPreludeActsProperly,
      topLevelToImportDoesntMatter,
      ambiSymbolInOpen,
      notAmbiIfLocalF,
      notAmbiIfTopLevel
      -- onlyOpenProperSymbols
    ]

--------------------------------------------------------------------------------
-- Setup Modules
--------------------------------------------------------------------------------

prelude :: IO (Env.New Context.T)
prelude = do
  ctx <- Context.empty (pure "Prelude")
  ctx
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
    |> pure

ourModule :: IO (Env.New Context.T)
ourModule = do
  prelude <- prelude
  mod <- Context.switchNameSpace (Context.topLevelName :| ["Londo"]) prelude
  case mod of
    Right ctx -> pure ctx
    Left ____ -> pure prelude

resolveOurModule :: IO (Either Resolve.Error Resolve.OpenMap)
resolveOurModule = do
  mod <- ourModule
  Resolve.resolve
    mod
    [ Resolve.Pre
        [Context.topLevelName :| ["Prelude"]]
        []
        (Context.topLevelName :| ["Londo"])
    ]
    |> Resolve.runM

sameSymbolModule :: IO (Env.New Context.T)
sameSymbolModule = do
  mod <- ourModule
  let added =
        Context.add (NameSpace.Pub "phantasm") (defaultDef "phantasm") mod
  --
  Right switched <-
    Context.switchNameSpace (Context.topLevelName :| ["Stirner"]) added
  --
  let added2 =
        Context.add (NameSpace.Pub "phantasm") (defaultDef "of-the-mind") switched
   in pure added2

defaultDef :: Symbol -> Env.New Context.Definition
defaultDef symb =
  Context.Def
    Nothing
    Nothing
    (Types.Like [] (Types.Primitive (Types.Prim (pure symb))) :| [])
    (Context.Pred Context.Right 10)

preludeAdded :: IO (Env.New Context.T)
preludeAdded = do
  mod <- sameSymbolModule
  let added =
        Context.add (NameSpace.Pub "Prelude") (defaultDef "") mod
  Right switched <-
    Context.switchNameSpace (Context.topLevelName :| ["Londo"]) added
  let added' =
        Context.add (NameSpace.Pub "Prelude") (defaultDef "") switched
  Right switched' <-
    Context.switchNameSpace (Context.topLevelName :| ["Max"]) added'
  let added'' =
        Context.add (NameSpace.Pub "phantasm") (defaultDef "") switched'
  pure added''

resolvePreludeAdded :: IO (Either Resolve.Error Resolve.OpenMap)
resolvePreludeAdded = do
  prelude <- preludeAdded
  [Resolve.Pre [pure "Stirner", pure "Londo"] [] (Context.topLevelName :| ["Max"])]
    |> Resolve.resolve prelude
    |> Resolve.runM

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

topLevelToImportDoesntMatter :: T.TestTree
topLevelToImportDoesntMatter =
  ( do
      mod <- ourModule
      resolvedTopWithPrelude <- resolveOurModule
      resolved <-
        Resolve.resolve
          mod
          [ Resolve.Pre
              [pure "Prelude"]
              []
              (Context.topLevelName :| ["Londo"])
          ]
          |> Resolve.runM
      resolvedTopWithPrelude T.@=? resolved
  )
    |> T.testCase
      "adding top level to module open does not matter if there is no lower module"

ambiSymbolInOpen :: T.TestTree
ambiSymbolInOpen =
  ( do
      sameSymbolModule <- sameSymbolModule
      Right switched <-
        Context.switchNameSpace (Context.topLevelName :| ["Max"]) sameSymbolModule
      resovled <-
        [Resolve.Pre [pure "Stirner", pure "Londo"] [] (Context.topLevelName :| ["Max"])]
          |> Resolve.resolve switched
          |> Resolve.runM
      resovled T.@=? Left (Resolve.AmbiguousSymbol "phantasm")
  )
    |> T.testCase
      "opening same symbol with it not being in the module def should be ambi"

notAmbiIfLocalF :: T.TestTree
notAmbiIfLocalF =
  ( do
      sameSymbolModule <- sameSymbolModule
      Right switched <-
        Context.switchNameSpace (Context.topLevelName :| ["Max"]) sameSymbolModule
      let added = Context.add (NameSpace.Pub "phantasm") (defaultDef "") switched
          opens =
            Map.fromList
              [ ( Context.topLevelName :| ["Max"],
                  [ Env.Explicit (Context.topLevelName :| ["Stirner"]),
                    Env.Explicit (Context.topLevelName :| ["Londo"])
                  ]
                )
              ]
      resovled <-
        [Resolve.Pre [pure "Stirner", pure "Londo"] [] (Context.topLevelName :| ["Max"])]
          |> Resolve.resolve added
          |> Resolve.runM
      resovled T.@=? Right opens
  )
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
   in ( do
          res <- resolvePreludeAdded
          res T.@=? Right opens
      )
        |> T.testCase
          "opening same symbol with it being in the module is not ambi"
-- onlyOpenProperSymbols :: T.TestTree
-- onlyOpenProperSymbols =
--   ( do
--       max <- Context.empty (pure "Max")
--       prelude <- preludeAdded
--       Right opens <- resolvePreludeAdded
--       (_, Env.Env {modMap}) <-
--         Env.bareRun
--           Env.populateModMap
--           max
--           prelude
--           opens
--       modMap T.@=? mempty
--   )
--     |> T.testCase
--       "explicit symbols don't get added to the symbol mapping"
