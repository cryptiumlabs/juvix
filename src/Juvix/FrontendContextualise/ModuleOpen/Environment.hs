{-# LANGUAGE LiberalTypeSynonyms #-}

module Juvix.FrontendContextualise.ModuleOpen.Environment
  ( module Juvix.FrontendContextualise.ModuleOpen.Environment,
    module Juvix.FrontendContextualise.Environment,
  )
where

import qualified Juvix.Core.Common.Context as Context
import Juvix.FrontendContextualise.Environment
import qualified Juvix.FrontendContextualise.InfixPrecedence.Types as Old
import qualified Juvix.FrontendContextualise.ModuleOpen.Types as New
import Juvix.Library
import qualified Juvix.Library.HashMap as Map

type Old f =
  f (NonEmpty (Old.FunctionLike Old.Expression)) Old.Signature Old.Type

type New f =
  f (NonEmpty (New.FunctionLike New.Expression)) New.Signature New.Type

type WorkingMaps m =
  ( HasState "old" (Old Context.T) m,
    HasState "new" (New Context.T) m,
    HasThrow "error" Error m,
    HasState "modMap" ModuleMap m
  )

data Environment
  = Env
      { old :: Old Context.T,
        new :: New Context.T,
        modMap :: ModuleMap
      }
  deriving (Generic)

type FinalContext = New Context.T

data Error
  = UnknownModule Symbol

type ContextAlias =
  ExceptT Error (State Environment)

type ModuleMap = Map.T Symbol (NonEmpty Symbol)

newtype Context a
  = Ctx {antiAlias :: ContextAlias a}
  deriving (Functor, Applicative, Monad)
  deriving
    ( HasState "old" (Old Context.T),
      HasSink "old" (Old Context.T),
      HasSource "old" (Old Context.T)
    )
    via StateField "old" ContextAlias
  deriving
    ( HasState "new" (New Context.T),
      HasSink "new" (New Context.T),
      HasSource "new" (New Context.T)
    )
    via StateField "new" ContextAlias
  deriving
    ( HasState "modMap" ModuleMap,
      HasSink "modMap" ModuleMap,
      HasSource "modMap" ModuleMap
    )
    via StateField "modMap" ContextAlias
  deriving
    (HasThrow "error" Error)
    via MonadError ContextAlias

runEnv :: Context a -> Old Context.T -> (Either Error a, Environment)
runEnv (Ctx c) old = runState (runExceptT c) (Env old Context.empty mempty)

-- for this function just the first part of the symbol is enough
qualifyName ::
  HasState "modMap" ModuleMap m => NonEmpty Symbol -> m (NonEmpty Symbol)
qualifyName sym@(s :| _) = do
  qualifieds <- get @"modMap"
  case qualifieds Map.!? s of
    Just preQualified ->
      pure $ preQualified <> sym
    Nothing ->
      pure sym

addModMap ::
  HasState "modMap" ModuleMap m => Symbol -> NonEmpty Symbol -> m ()
addModMap toAdd qualification =
  Juvix.Library.modify @"modMap" (Map.insert toAdd qualification)

lookupModMap ::
  HasState "modMap" ModuleMap m => Symbol -> m (Maybe (NonEmpty Symbol))
lookupModMap s =
  (Map.!? s) <$> get @"modMap"

removeModMap ::
  HasState "modMap" ModuleMap m => Symbol -> m ()
removeModMap s = Juvix.Library.modify @"modMap" (Map.delete s)
