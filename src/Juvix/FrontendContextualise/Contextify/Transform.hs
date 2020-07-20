{-# LANGUAGE LiberalTypeSynonyms #-}

module Juvix.FrontendContextualise.Contextify.Transform where

import qualified Juvix.Core.Common.Context as Context
import qualified Juvix.FrontendDesugar.RemoveDo.Types as Repr
import Juvix.Library

type Repr f =
  f (NonEmpty (Repr.FunctionLike Repr.Expression)) Repr.Signature Repr.Type

type Context =
  Repr Context.T

type Definition =
  Repr Context.Definition

contextify :: [Repr.TopLevel] -> Context
contextify = undefined

-- for now we'll drop typeclass and it's instance
-- for function top level we have to determine if it's a record
updateTopLevel :: Repr.TopLevel -> Context -> Context
updateTopLevel (Repr.Type t@(Repr.Typ _ name _ _)) ctx =
  Context.add name (Context.TypeDeclar t) ctx
updateTopLevel (Repr.Function t@(Repr.Func name f sig)) ctx =
  Context.add name (decideRecordOrDef f sig) ctx
updateTopLevel Repr.TypeClass ctx = ctx
updateTopLevel Repr.TypeClassInstance ctx = ctx

-- | decideRecordOrDef tries to figure out
-- if a given defintiion is a record or a definition
decideRecordOrDef ::
  NonEmpty (Repr.FunctionLike Repr.Expression) -> Maybe Repr.Signature -> Definition
decideRecordOrDef xs ty
  | len == 1 = undefined
  | otherwise =
    Context.Def Nothing ty xs Context.default'
  where
    len = length xs
