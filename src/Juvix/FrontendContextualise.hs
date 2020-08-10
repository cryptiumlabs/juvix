{-# LANGUAGE LiberalTypeSynonyms #-}

-- |
-- - order of Passes
--   1. =ModuleOpen=
--   2. =InfixPrecedence=
module Juvix.FrontendContextualise where

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

contextualize :: [Initial.TopLevel] -> NameSymbol.T -> Either Error Target.FinalContext
contextualize arg sym =
  case Module.transformContext (Contextify.contextify sym arg) of
    Left err -> Left (ModuleErr err)
    Right xs ->
      case Infix.transformContext xs of
        Left err -> Left (InfixErr err)
        Right xs -> Right xs
