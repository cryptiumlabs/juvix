{-# LANGUAGE LiberalTypeSynonyms #-}

module FrontendContextualise.Module.Open where

import qualified Data.Text as Text
import qualified Juvix.Core.Common.Context as Context
import qualified Juvix.Core.Common.NameSpace as NameSpace
import qualified Juvix.FrontendContextualise.ModuleOpen.Environment as Env
import qualified Juvix.FrontendContextualise.ModuleOpen.Types as Types
import Juvix.Library
import qualified Juvix.Library.HashMap as HashMap

-- import qualified Test.Tasty as T
-- import qualified Test.Tasty.HUnit as T

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

ourModule =
  case Context.switchNameSpace (Context.topLevelName :| ["Londo"]) prelude of
    Right ctx -> ctx
    Left ____ -> prelude

firstOpen =
  Env.resolve ourModule [Env.Pre [(Context.topLevelName :| ["Prelude"])] [] (Context.topLevelName :| ["Londo"])]
