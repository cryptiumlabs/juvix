{-# LANGUAGE LiberalTypeSynonyms #-}

-- |
-- - order of Passes
--   1. =ModuleOpen=
--   2. =InfixPrecedence=
module Juvix.FrontendContextualise where

import qualified Juvix.Core.Common.Context as Context
import qualified Juvix.Core.Common.NameSymbol as NameSymbol
import qualified Juvix.FrontendContextualise.Contextify.Transform as Contextify
import qualified Juvix.FrontendContextualise.InfixPrecedence.Environment as Target
import qualified Juvix.FrontendContextualise.InfixPrecedence.Environment as Infix
import qualified Juvix.FrontendContextualise.InfixPrecedence.Transform as Infix
import qualified Juvix.FrontendContextualise.ModuleOpen.Environment as Module
import qualified Juvix.FrontendContextualise.ModuleOpen.Transform as Module
import qualified Juvix.FrontendDesugar.RemoveDo.Types as Initial
import Juvix.Library

data Error
  = ModuleErr Module.Error
  | InfixErr Infix.Error
  | PathErr Context.PathError
  deriving (Show)

f ::
  NonEmpty (NameSymbol.T, [Initial.TopLevel]) -> Either Error Target.FinalContext
f = contextualize

contextualize ::
  NonEmpty (NameSymbol.T, [Initial.TopLevel]) -> Either Error Target.FinalContext
contextualize ((sym, xs) :| t) =
  case foldM
         Contextify.contextify
         (foldr Contextify.updateTopLevel (Context.empty sym) xs)
         t
  of
    Left err -> Left (PathErr err)
    Right context ->
      case Module.transformContext context of
        Left err -> Left (ModuleErr err)
        Right xs ->
          case Infix.transformContext xs of
            Left err -> Left (InfixErr err)
            Right xs -> Right xs
