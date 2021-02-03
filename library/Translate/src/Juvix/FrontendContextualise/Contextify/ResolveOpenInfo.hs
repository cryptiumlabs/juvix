module Juvix.FrontendContextualise.Contextify.ResolveOpenInfo where

import Control.Lens hiding ((|>))
import qualified Juvix.Core.Common.Context as Context
import qualified Juvix.Core.Common.Open as Open
import Juvix.Library
import qualified Juvix.Library.HashMap as HashMap
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified StmContainers.Map as STM

data PreQualified
  = Pre
      { opens :: [NameSymbol.T],
        implicitInner :: [NameSymbol.T],
        explicitModule :: NameSymbol.T
      }
  deriving (Show, Eq)

data Error
  = UnknownModule NameSymbol.T
  | CantResolveModules [NameSymbol.T]
  deriving (Show, Eq)

type RunM =
  ExceptT Error IO

newtype M a = M (RunM a)
  deriving (Functor, Applicative, Monad, MonadIO)
  deriving (HasThrow "left" Error) via MonadError RunM

runM :: M a -> IO (Either Error a)
runM (M a) = runExceptT a

run ::
  Context.T term ty sumRep ->
  [PreQualified] ->
  IO (Either Error (Context.T term ty sumRep))
run ctx qualifieds = do
  resolved <- resolve ctx qualifieds
  case resolved of
    Right reverse -> pure (Right ctx {Context.reverseLookup = reverse})
    Left error -> pure (Left error)

resolve :: Context.T a b c -> [PreQualified] -> IO (Either Error Context.ReverseLookup)
resolve ctx = runM . foldM (resolveSingle ctx) mempty

resolveSingle ::
  (HasThrow "left" Error m, MonadIO m) =>
  Context.T a b c ->
  Context.ReverseLookup ->
  PreQualified ->
  m Context.ReverseLookup
resolveSingle ctx reverseLookup Pre {opens, implicitInner, explicitModule} =
  case nameSpace of
    Nothing ->
      throw @"left" (UnknownModule explicitModule)
    Just ctx -> do
      let symbolMap =
            ctx ^. Context._currentNameSpace . Context.qualifiedMap
          updateRevLook impExplict existing =
            let whousesInfo =
                  Context.Who {impExplict, modName = explicitModule, symbolMap}
             in case existing of
                  Nothing -> Just [whousesInfo]
                  Just es -> Just (whousesInfo : es)
          alterUpdate = HashMap.alter . updateRevLook
          newResolve =
            foldr (alterUpdate Open.Explicit) reverseLookup opens
              |> \lookup ->
                foldr (alterUpdate Open.Implicit) lookup implicitInner
      -- TODO ∷ put logic that mutates the local opens here
      undefined
      pure newResolve
  where
    nameSpace = Context.inNameSpace explicitModule ctx

grabQualifiedMap ::
  HasThrow "left" Error m => Context.T a b c -> NameSymbol.T -> m Context.SymbolMap
grabQualifiedMap ctx name =
  case Context.extractValue <$> Context.lookup name ctx of
    Just (Context.Record Context.Rec {recordQualifiedMap}) ->
      pure recordQualifiedMap
    _ -> throw @"left" (UnknownModule (Context.qualifyName name ctx))
