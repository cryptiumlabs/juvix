module FrontendContextualise.Module.Open where

import Juvix.Library
import qualified Juvix.FrontendContextualise.ModuleOpen.Environment as Env
import qualified Data.Text as Text
import qualified Juvix.Core.Common.Context as Context
import qualified Juvix.Core.Common.NameSpace as NameSpace
import qualified Juvix.Library.HashMap as HashMap
-- import qualified Test.Tasty as T
-- import qualified Test.Tasty.HUnit as T


prelude =
  Context.empty (pure "Prelude")
  |> Context.add
      (NameSpace.Pub "->")
      (Context.Def Nothing Nothing 3 (Context.Pred Context.Right 0))
  |> Context.add
      (NameSpace.Pub ":")
      (Context.Def Nothing Nothing 1 (Context.Pred Context.Right 2))



ourModule =
  case Context.switchNameSpace (Context.topLevelName :| ["Londo"]) prelude of
    Right ctx -> ctx
    Left _ -> prelude

firstOpen =
  Env.resolve ourModule [Env.Pre [(Context.topLevelName :| ["Prelude"])] [] (Context.topLevelName :| ["Londo"])]
