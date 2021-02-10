{-# LANGUAGE LiberalTypeSynonyms #-}

module Contextualise.Module.Resolve where

import qualified Contextualise.Module.Open as Open
import Control.Lens hiding ((|>))
import qualified Juvix.Core.Common.Context as Context
import qualified Juvix.Core.Common.Context as Ctx
import qualified Juvix.Core.Common.NameSpace as NameSpace
import qualified Juvix.FrontendContextualise.Contextify.ResolveOpenInfo as Resolve
import qualified Juvix.FrontendContextualise.ModuleOpen.Environment as Env
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
    [ preludeProperlyOpens,
      ambigiousPrelude,
      explicitOpenBeatsImplicit,
      onlyOpenProperSymbols
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

ambigiousPrelude :: T.TestTree
ambigiousPrelude =
  T.testCase
    "explicitly opening two modules with the same exports errors:"
    ( do
        -- this module has Stirner and Londo both have Prelude inside them!
        prelude <- Open.preludeAdded
        reality <-
          Resolve.run
            prelude
            [ Resolve.Pre
                [pure "Stirner", pure "Londo"]
                []
                (Context.topLevelName :| ["Max"])
            ]
        let expected =
              Left
                ( Resolve.ModuleConflict
                    "Prelude"
                    ["TopLevel" :| ["Londo"], "TopLevel" :| ["Stirner"]]
                )
        reality T.@=? expected
    )

explicitOpenBeatsImplicit :: T.TestTree
explicitOpenBeatsImplicit =
  T.testCase
    "the implicit definitions get overwritten by the Explicit ones"
    ( do
        prelude <- preludeAddedPlusExtra
        let manualImplicit =
              Map.fromList
                [ ( Ctx.topLevelName :| ["Max"],
                    [ Env.Explicit (Ctx.topLevelName :| ["Stirner"]),
                      Env.Implicit (Ctx.topLevelName :| ["Londo"])
                    ]
                  )
                ]
        Right () <- manualPopulate prelude manualImplicit
        Right stmMap <-
          Resolve.runM $
            Resolve.grabQualifiedMap prelude (Context.topLevelName :| ["Max"])
        reality <- stmMapToList stmMap
        reality T.@=? expected
    )
  where
    expected =
      [ ("mollari", Ctx.SymInfo {used = Ctx.NotUsed, mod = "TopLevel" :| ["Londo"]}),
        ("Prelude", Ctx.SymInfo {used = Ctx.NotUsed, mod = "TopLevel" :| ["Stirner"]})
      ]

onlyOpenProperSymbols :: T.TestTree
onlyOpenProperSymbols =
  T.testCase
    "explicit symbols don't get added to the symbol mapping"
    ( do
        prelude <- Open.preludeAdded
        Right ctx <-
          Resolve.run
            prelude
            [Resolve.Pre [pure "Stirner"] [] (Context.topLevelName :| ["Max"])]
        Right stmMap <-
          Resolve.runM $ Resolve.grabQualifiedMap ctx (Context.topLevelName :| ["Max"])
        reality <- stmMapToList stmMap
        reality T.@=? expected
    )
  where
    expected =
      [("Prelude", Ctx.SymInfo {used = Ctx.NotUsed, mod = "TopLevel" :| ["Stirner"]})]

stmMapToList :: STM.Map key value -> IO [(key, value)]
stmMapToList = atomically . ListT.toList . STM.listT

-- TODO ∷ remove when proper implicit opens are done
manualPopulate :: Ctx.T a b c -> Resolve.OpenMap -> IO (Either Resolve.Error ())
manualPopulate ctx fullyQualifiedResolves = do
  Resolve.runM $ do
    Resolve.populateOpens fullyQualifiedResolves ctx

preludeAddedPlusExtra :: IO (Env.New Context.T)
preludeAddedPlusExtra = do
  prelude <- Open.preludeAdded
  let Just ctx =
        Context.inNameSpace (Context.topLevelName :| ["Londo"]) prelude
  pure $ Context.add (NameSpace.Pub "mollari") (Open.defaultDef quote) ctx
  where
    quote =
      "Isn't it strange, G'Kar? When we first met I had no power and\
      \ all the choices I could ever want. And now I have all the power \
      \ I could ever want and no choices at all. No choice at all."
