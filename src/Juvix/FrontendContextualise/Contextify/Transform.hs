module Juvix.FrontendContextualise.Contextify.Transform where

import Juvix.Library

import qualified Juvix.FrontendDesugar.RemoveDo.Types as Repr
import qualified Juvix.Core.Common.Context as Context


type Context =
  Context.T (NonEmpty (Repr.FunctionLike Repr.Expression)) Repr.Signature Repr.Type

contextify :: [Repr.TopLevel] -> Context
contextify = undefined


-- for now we'll drop typeclass and it's instance
-- for function top level we have to determine if it's a record
updateTopLevel :: Repr.TopLevel -> Context -> Context
updateTopLevel (Repr.Type t@(Repr.Typ _ name _ _)) ctx =
  Context.add name (Context.TypeDeclar t) ctx
updateTopLevel (Repr.Function t@(Repr.Func name f sig)) ctx =
  Context.add name undefined ctx
updateTopLevel Repr.TypeClass ctx = ctx
updateTopLevel Repr.TypeClassInstance ctx = ctx
