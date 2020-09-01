{-# LANGUAGE LiberalTypeSynonyms #-}

module Juvix.FrontendContextualise.ModuleOpen.Environment
  ( module Juvix.FrontendContextualise.ModuleOpen.Environment,
    module Juvix.FrontendContextualise.Environment,
  )
where

import qualified Data.HashSet as Set
import qualified Juvix.Core.Common.Context as Context
import qualified Juvix.Core.Common.NameSpace as NameSpace
import qualified Juvix.Core.Common.NameSymbol as NameSymbol
import Juvix.FrontendContextualise.Environment
import qualified Juvix.FrontendContextualise.ModuleOpen.Types as New
import qualified Juvix.FrontendDesugar.RemoveDo.Types as Old
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
    HasState "modMap" ModuleMap m,
    HasReader "openMap" OpenMap m
  )

data Environment
  = Env
      { old :: Old Context.T,
        new :: New Context.T,
        modMap :: ModuleMap,
        openMap :: OpenMap
      }
  deriving (Generic)

type FinalContext = New Context.T

data Error
  = UnknownModule NameSymbol.T
  | AmbigiousSymbol Symbol
  | OpenNonModule (Set.HashSet Context.NameSymbol)
  | IllegalModuleSwitch Context.NameSymbol
  | ConflictingSymbols Context.NameSymbol
  | CantResolveModules [NameSymbol.T]
  deriving (Show)

data Open a
  = Implicit a
  | Explicit a
  deriving (Show, Eq, Ord)

type ContextAlias =
  ExceptT Error (State Environment)

type ModuleMap = Map.T Symbol NameSymbol.T

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
  deriving
    ( HasReader "openMap" OpenMap,
      HasSource "openMap" OpenMap
    )
    via ReaderField "openMap" ContextAlias

--------------------------------------------------------------------------------
-- Types for resolving opens
--------------------------------------------------------------------------------
-- - before we are able to qaulify all symbols, we need the context at
--   a fully realized state.
-- - This hosts
--   1. the module
--   2. the inner modules (which thus have implciit opens of all
--      opens)
--   3. All opens
-- - Since we desugar all modules to records, we can't have opens over
--   them, hence no need to store it separately
-- - Any resolution will thus happen at the explicit module itself, as
--   trying to do so in the inner modules would lead to a path error

data PreQualified
  = Pre
      { opens :: [NameSymbol.T],
        implicitInner :: [NameSymbol.T],
        explicitModule :: NameSymbol.T
      }
  deriving (Show, Eq)

type OpenMap = Map.T Context.NameSymbol [Open NameSymbol.T]

data Resolve a b c
  = Res
      { resolved :: [(Context.From (Context.Definition a b c), NameSymbol.T)],
        notResolved :: [NameSymbol.T]
      }
  deriving (Show)

--------------------------------------------------------------------------------
-- Running functions
--------------------------------------------------------------------------------

runEnv ::
  Context a -> Old Context.T -> [PreQualified] -> (Either Error a, Environment)
runEnv (Ctx c) old pres =
  case resolve old pres of
    Right opens ->
      Env old (Context.empty (Context.currentName old)) mempty opens
        |> runState (runExceptT c)
    Left err -> (Left err, undefined)

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

--------------------------------------------------------------------------------
-- fully resolve module opens
--------------------------------------------------------------------------------

-- Precondition ∷ old and new context must be in the same context
-- also terms must be in either the old map or the new map
-- any place where this is inconsistent will break resolution

-- | @populateModMap@ populates the modMap with all the global opens
-- in the module
populateModMap ::
  WorkingMaps m => m ()
populateModMap = do
  old <- get @"old"
  new <- get @"new"
  open <- Juvix.Library.ask @"openMap"
  modM <- get @"modMap"
  let curr = pure Context.topLevelName <> Context.currentName old
  case open Map.!? curr of
    Nothing ->
      pure ()
    Just opens ->
      -- TODO ∷
      -- explicts and implicits for now work the same
      put @"modMap" (foldr f modM opens)
      where
        f y modMap = foldr addNewSymbol modMap openList
          where
            nameSpace =
              case y of
                Implicit x -> x
                Explicit x -> x
            unpopulate = (() <$)
            --
            addNewSymbol x modMap =
              case unpopulate (Context.lookup (NameSymbol.fromSymbol x) old)
                <|> unpopulate (Context.lookup (NameSymbol.fromSymbol x) new) of
                Just () ->
                  -- if the symbol is in the map, then
                  -- we don't shadow it....
                  -- TODO ∷ later throw an error for
                  --        explicit opens that do this
                  modMap
                Nothing ->
                  Map.insert x nameSpace modMap
            --
            openList =
              -- haskell gives type error if grabList is not typed
              let grabList ::
                    Context.T a b c ->
                    NameSpace.List (Context.Definition a b c)
                  grabList ctx =
                    case Context.extractValue <$> Context.lookup nameSpace ctx of
                      Just (Context.Record nameSpace _) ->
                        NameSpace.toList nameSpace
                      Just _ ->
                        NameSpace.List [] []
                      Nothing ->
                        NameSpace.List [] []
                  NameSpace.List {publicL = pubN} =
                    grabList new
                  NameSpace.List {publicL = pubO} =
                    grabList old
               in fmap fst pubN <> fmap fst pubO

-- we don't take a module mapping as all symbols at the point of
-- resolve can't be anything else, thus we don't have to pre-pend any
-- qualification at this point... we add qualification later as we add
-- things, as we have to figure out the full resolution of all opens

-- | @resolve@ takes a context and a list of open modules and the modules
-- they are open in (the module itself, and sub modules, which the opens are
-- implicit), and fully resolves the opens to their full name.
-- for example @open Prelude@ in the module Londo translates to
-- @resolve ctx PreQualified {opens = [Prelude]}@
-- == @Right (fromList [(TopLevel :| [Londo],[Explicit (TopLevel :| [Prelude])])])@
resolve :: Context.T a b c -> [PreQualified] -> Either Error OpenMap
resolve ctx = foldM (resolveSingle ctx) mempty

resolveSingle ::
  Context.T a b c -> OpenMap -> PreQualified -> Either Error OpenMap
resolveSingle ctx openMap Pre {opens, implicitInner, explicitModule} =
  case Context.switchNameSpace explicitModule ctx of
    Left (Context.VariableShared err) ->
      Left (IllegalModuleSwitch err)
    Right ctx -> do
      resolved <- pathsCanBeResolved ctx opens
      qualifiedNames <- resolveLoop ctx mempty resolved
      openMap
        |> Map.insert explicitModule (fmap Explicit qualifiedNames)
        |> ( \map ->
               foldr
                 (\mod -> Map.insert mod (fmap Implicit qualifiedNames))
                 map
                 implicitInner
           )
        |> Right

-- this goes off after the checks have passed regarding if the paths
-- are even possible to resolve
resolveLoop ::
  Context.T a b c -> ModuleMap -> Resolve a b c -> Either Error [NameSymbol.T]
resolveLoop ctx map Res {resolved, notResolved = cantResolveNow} = do
  map <- foldM addToModMap map fullyQualifyRes
  --
  let newResolve = resolveWhatWeCan ctx (qualifyCant map <$> cantResolveNow)
  --
  if  | length cantResolveNow == 0 ->
        Right qualifedAns
      | length (notResolved newResolve) == length cantResolveNow ->
        Left (CantResolveModules cantResolveNow)
      | otherwise ->
        (qualifedAns <>) <$> resolveLoop ctx map newResolve
  where
    fullyQualifyRes =
      Context.resolveName ctx <$> resolved
    qualifedAns =
      fmap snd fullyQualifyRes
    addToModMap map (def, sym) =
      case def of
        Context.Record cont _ ->
          let NameSpace.List {publicL} = NameSpace.toList cont
           in foldlM
                ( \map (x, _) ->
                    case map Map.!? x of
                      -- this means we already have opened this in another
                      -- module
                      Just _ -> Left (AmbigiousSymbol x)
                      -- if it's already in scope from below or above us
                      -- don't add to the remapping
                      Nothing ->
                        case Context.lookup (pure x) ctx of
                          Just _ -> Right map
                          Nothing -> Right (Map.insert x sym map)
                )
                map
                publicL
        -- should be updated when we can open expressions
        _ ->
          Left (UnknownModule sym)
    qualifyCant newMap term =
      case newMap Map.!? NameSymbol.hd term of
        Nothing ->
          term
        Just x ->
          x <> term

-- Since Locals and top level beats opens, we can determine from the
-- start that any module which tries to open a nested path fails,
-- yet any previous part succeeds, that an illegal open is happening,
-- and we can error out immediately

-- | @pathsCanBeResolved@ takes a context and a list of opens,
-- we then try to resolve if the opens are legal, if so we return
-- a list of ones that can be determined now, and a list to be resolved
pathsCanBeResolved ::
  Context.T a b c -> [NameSymbol.T] -> Either Error (Resolve a b c)
pathsCanBeResolved ctx opens
  | fmap firstName (notResolved resFull) == notResolved resFirst =
    Right resFull
  | otherwise =
    Left (OpenNonModule diff)
  where
    resFull = resolveWhatWeCan ctx opens
    resFirst = resolveWhatWeCan ctx (firstName <$> opens)
    -- O(n log₁₆(n))
    diff =
      Set.difference
        (Set.fromList (notResolved resFull))
        (Set.fromList (notResolved resFirst))
        <> Set.difference
          (Set.fromList (notResolved resFirst))
          (Set.fromList (notResolved resFull))

resolveWhatWeCan :: Context.T a b c -> [NameSymbol.T] -> Resolve a b c
resolveWhatWeCan ctx opens = Res {resolved, notResolved}
  where
    dupLook =
      fmap (\openMod -> (Context.lookup openMod ctx, openMod))
    (resolved, notResolved) =
      splitMaybes (dupLook opens)

----------------------------------------
-- Helpers for resolve
----------------------------------------

splitMaybes :: [(Maybe a, b)] -> ([(a, b)], [b])
splitMaybes = foldr f ([], [])
  where
    f (Just a, b) = first ((a, b) :)
    f (Nothing, b) = second (b :)

firstName :: NameSymbol.T -> NameSymbol.T
firstName (x :| y : _)
  | Context.topLevelName == x =
    x :| [y]
  | otherwise =
    NameSymbol.fromSymbol y
firstName xs =
  NameSymbol.hd xs
    |> NameSymbol.fromSymbol
