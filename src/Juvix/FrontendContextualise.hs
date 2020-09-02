{-# LANGUAGE LiberalTypeSynonyms #-}

-- |
-- - order of Passes
--   1. =ModuleOpen=
--   2. =InfixPrecedence=
module Juvix.FrontendContextualise
  (module Juvix.FrontendContextualise,
   Target.FinalContext)
where

import qualified Juvix.Core.Common.Context as Context
import qualified Juvix.Core.Common.NameSymbol as NameSymbol
import qualified Juvix.FrontendContextualise.Contextify.Transform as Contextify
import qualified Juvix.FrontendContextualise.Contextify.Types as Contextify
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

type Final f = Target.New f

op ::
  NonEmpty (NameSymbol.T, [Initial.TopLevel]) -> Either Error Target.FinalContext
op = contextualize

contextualize ::
  NonEmpty (NameSymbol.T, [Initial.TopLevel]) -> Either Error Target.FinalContext
contextualize init =
  case contextify init of
    Left err -> Left (PathErr err)
    Right (context, openList) ->
      case Module.transformContext context openList of
        Left err -> Left (ModuleErr err)
        Right xs ->
          case Infix.transformContext xs of
            Left err -> Left (InfixErr err)
            Right xs -> Right xs

contextify ::
  NonEmpty (NameSymbol.T, [Initial.TopLevel]) ->
  Either Context.PathError (Contextify.Context, [Module.PreQualified])
contextify t@((sym, _) :| _) =
  foldM resolveOpens (Context.empty sym, []) (addTop <$> t)

addTop :: Bifunctor p => p NameSymbol.T c -> p NameSymbol.T c
addTop = first (NameSymbol.cons Context.topLevelName)

-- we get the opens
resolveOpens ::
  (Contextify.Context, [Module.PreQualified]) ->
  (Context.NameSymbol, [Initial.TopLevel]) ->
  Either
    Context.PathError
    (Contextify.Context, [Module.PreQualified])
resolveOpens (ctx', openList) (sym, xs) =
  case Contextify.f ctx' (sym, xs) of
    Right Contextify.P {ctx, opens, modsDefined} ->
      Right
        ( ctx,
          Module.Pre
            { opens,
              explicitModule = sym,
              implicitInner = modsDefined
            }
            : openList
        )
    Left err -> Left err
