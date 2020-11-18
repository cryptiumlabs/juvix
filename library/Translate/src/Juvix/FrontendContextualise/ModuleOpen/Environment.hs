{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Juvix.FrontendContextualise.ModuleOpen.Environment
  ( module Juvix.FrontendContextualise.ModuleOpen.Environment,
    module Juvix.FrontendContextualise.Environment,
  )
where

import qualified Data.HashSet as Set
import Data.Kind (Constraint)
import qualified Juvix.Core.Common.Context as Context
import qualified Juvix.Core.Common.NameSpace as NameSpace
import Juvix.FrontendContextualise.Environment
import qualified Juvix.FrontendContextualise.ModuleOpen.Types as New
import qualified Juvix.FrontendDesugar.RemoveDo.Types as Old
import Juvix.Library
import qualified Juvix.Library.HashMap as Map
import qualified Juvix.Library.NameSymbol as NameSymbol

--------------------------------------------------------------------------------
-- Type Aliases and effect setup
--------------------------------------------------------------------------------

type ModuleMap = Map.T Symbol NameSymbol.T

type Old f =
  f (NonEmpty (Old.FunctionLike Old.Expression)) Old.Signature Old.Type

type New f =
  f (NonEmpty (New.FunctionLike New.Expression)) New.Signature New.Type

type TransitionMap m =
  (HasState "old" (Old Context.T) m, HasState "new" (New Context.T) m)

type SingleMap a b c m =
  (HasState "env" (Context.T a b c) m, HasState "new" (New Context.T) m)

type WorkingMaps m =
  (TransitionMap m, Expression EnvDispatch m)

type ModuleNames tag m =
  (NameConstraint tag m, HasReader "dispatch" tag m, Names tag)

type ModuleSwitch tag m =
  (SwitchConstraint tag m, HasReader "dispatch" tag m, Switch tag)

-- The effect of expression and below note that new is not the new map
-- per se, but more instead of where local functions get added...  for
-- a full pass this is indeed the new context, but for a single pass,
-- just a local cache
type Expression tag m =
  ( HasState "new" (New Context.T) m,
    ModuleNames tag m,
    HasThrow "error" Error m,
    HasState "modMap" ModuleMap m,
    HasReader "openMap" OpenMap m,
    ModuleSwitch tag m
  )

--------------------------------------------------------------------------------
-- Environment data declarations
--------------------------------------------------------------------------------

-- | the traditional transition between one context and another
data EnvDispatch = EnvDispatch deriving (Show)

-- | Used when going trying to transition one definition to the next level
data SingleDispatch (a :: Type) (b :: Type) (c :: Type) = SingleDispatch

-- | @SingleEnv@ is used when we are trying to convert one function to the next step
data SingleEnv term1 ty1 sumRep1
  = Single
      { env :: Context.T term1 ty1 sumRep1,
        temp :: New Context.T,
        disp :: SingleDispatch term1 ty1 sumRep1,
        modM :: ModuleMap,
        openM :: OpenMap
      }
  deriving (Generic)

-- | @Environment@ is the environment used when going from one entire
-- phase to another
data Environment
  = Env
      { old :: Old Context.T,
        new :: New Context.T,
        modMap :: ModuleMap,
        openMap :: OpenMap,
        dispatch :: EnvDispatch
      }
  deriving (Generic, Show)

data Error
  = UnknownModule NameSymbol.T
  | AmbiguousSymbol Symbol
  | OpenNonModule (Set.HashSet Context.NameSymbol)
  | IllegalModuleSwitch Context.NameSymbol
  | ConflictingSymbols Context.NameSymbol
  | CantResolveModules [NameSymbol.T]
  deriving (Show, Eq)

data Open a
  = Implicit a
  | Explicit a
  deriving (Show, Eq, Ord)

type ContextAlias =
  ExceptT Error (State Environment)

newtype Context a = Ctx {antiAlias :: ContextAlias a}
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
    ( HasReader "dispatch" EnvDispatch,
      HasSource "dispatch" EnvDispatch
    )
    via ReaderField "dispatch" ContextAlias
  deriving
    ( HasReader "openMap" OpenMap,
      HasSource "openMap" OpenMap
    )
    via ReaderField "openMap" ContextAlias

type SingleAlias term1 ty1 sumRep1 =
  ExceptT Error (State (SingleEnv term1 ty1 sumRep1))

newtype SingleCont term1 ty1 sumRep1 a
  = SCtx {aSingle :: SingleAlias term1 ty1 sumRep1 a}
  deriving (Functor, Applicative, Monad)
  deriving
    ( HasState "new" (New Context.T),
      HasSink "new" (New Context.T),
      HasSource "new" (New Context.T)
    )
    via Rename "temp" (StateField "temp" (SingleAlias term1 ty1 sumRep1))
  deriving
    ( HasState "env" (Context.T term1 ty1 sumRep1),
      HasSink "env" (Context.T term1 ty1 sumRep1),
      HasSource "env" (Context.T term1 ty1 sumRep1)
    )
    via (StateField "env" (SingleAlias term1 ty1 sumRep1))
  deriving
    ( HasReader "dispatch" (SingleDispatch term1 ty1 sumRep1),
      HasSource "dispatch" (SingleDispatch term1 ty1 sumRep1)
    )
    via Rename "disp" (ReaderField "disp" (SingleAlias term1 ty1 sumRep1))
  deriving
    (HasThrow "error" Error)
    via MonadError (SingleAlias term1 ty1 sumRep1)
  deriving
    ( HasReader "openMap" OpenMap,
      HasSource "openMap" OpenMap
    )
    via Rename "openM" (ReaderField "openM" (SingleAlias term1 ty1 sumRep1))
  deriving
    ( HasState "modMap" ModuleMap,
      HasSink "modMap" ModuleMap,
      HasSource "modMap" ModuleMap
    )
    via Rename "modM" (StateField "modM" (SingleAlias term1 ty1 sumRep1))

--------------------------------------------------------------------------------
-- Generic Interface Definitions for Environment lookup
--------------------------------------------------------------------------------

class Switch a where
  type SwitchConstraint a (m :: * -> *) :: Constraint
  switch :: SwitchConstraint a m => Context.NameSymbol -> a -> m ()

-- | Names encapsulates the idea of looking up all current names in
-- a context
class Names a where
  type NameConstraint a (m :: * -> *) :: Constraint
  contextNames :: NameConstraint a m => NameSymbol.T -> a -> m [Symbol]
  currentNameSpace' :: NameConstraint a m => a -> m NameSymbol.T
  inCurrentModule' :: NameConstraint a m => NameSymbol.T -> a -> m Bool

instance Switch (SingleDispatch a b c) where
  type SwitchConstraint _ m = (SingleMap a b c m, HasThrow "error" Error m)
  switch sym SingleDispatch = do
    tmp <- get @"new"
    env <- get @"env"
    switchContextErr sym tmp >>= put @"new"
    switchContextErr sym env >>= put @"env"

instance Switch EnvDispatch where
  type SwitchConstraint _ m = (TransitionMap m, HasThrow "error" Error m)
  switch sym EnvDispatch = do
    -- no modifyM to remove this pattern
    old <- get @"old"
    new <- get @"new"
    switchContextErr sym old >>= put @"old"
    switchContextErr sym new >>= put @"new"

instance Names (SingleDispatch a b c) where
  type NameConstraint _ m = SingleMap a b c m

  -- arbitrary choice between new and env
  currentNameSpace' SingleDispatch =
    get @"env" >>| Context.currentName

  contextNames name SingleDispatch = do
    env <- get @"env"
    new <- get @"new"
    pure $ combineLists (grabList name env) (grabList name new)

  inCurrentModule' name SingleDispatch = do
    env <- get @"env"
    new <- get @"new"
    pure (inMap name env || inMap name new)

instance Names EnvDispatch where
  type NameConstraint _ m = TransitionMap m
  contextNames name EnvDispatch = do
    old <- get @"old"
    new <- get @"new"
    pure $ combineLists (grabList name new) (grabList name old)

  -- arbitrary choice between new and old
  currentNameSpace' EnvDispatch =
    get @"new" >>| Context.currentName

  inCurrentModule' name EnvDispatch = do
    old <- get @"old"
    new <- get @"new"
    pure (inMap name old || inMap name new)

switchNameSpace :: ModuleSwitch tag m => NameSymbol.T -> m ()
switchNameSpace name = Juvix.Library.ask @"dispatch" >>= switch name

inScopeNames :: ModuleNames tag m => NameSymbol.T -> m [Symbol]
inScopeNames name = Juvix.Library.ask @"dispatch" >>= contextNames name

currentNameSpace :: ModuleNames tag m => m NameSymbol.T
currentNameSpace = Juvix.Library.ask @"dispatch" >>= currentNameSpace'

inCurrentModule :: ModuleNames tag m => NameSymbol.T -> m Bool
inCurrentModule name = Juvix.Library.ask @"dispatch" >>= inCurrentModule' name

----------------------------------------
-- Helper functions for the instances
----------------------------------------

switchContextErr ::
  HasThrow "error" Error m =>
  NameSymbol.T ->
  Context.T term ty sumRep ->
  m (Context.T term ty sumRep)
switchContextErr sym ctx =
  case Context.switchNameSpace sym ctx of
    -- bad Error for now
    Left ____ -> throw @"error" (UnknownModule sym)
    Right map -> pure map

grabList :: NameSymbol.T -> Context.T a b c -> NameSpace.List (Context.Definition a b c)
grabList name ctx =
  case Context.extractValue <$> Context.lookup name ctx of
    Just (Context.Record nameSpace _) ->
      NameSpace.toList nameSpace
    Just _ ->
      NameSpace.List [] []
    Nothing ->
      NameSpace.List [] []

combineLists :: NameSpace.List b1 -> NameSpace.List b2 -> [Symbol]
combineLists NameSpace.List {publicL = pub1} NameSpace.List {publicL = pub2} =
  fmap fst pub1 <> fmap fst pub2

inMap :: NameSymbol.T -> Context.T term ty sumRep -> Bool
inMap name ctx = isJust (Context.lookup name ctx)

type FinalContext = New Context.T

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

bareRun ::
  Context a ->
  Old Context.T ->
  New Context.T ->
  OpenMap ->
  (Either Error a, Environment)
bareRun (Ctx c) old new opens =
  Env old new mempty opens EnvDispatch
    |> runState (runExceptT c)

runEnv ::
  Context a -> Old Context.T -> [PreQualified] -> (Either Error a, Environment)
runEnv (Ctx c) old pres =
  case resolve old pres of
    Right opens ->
      Env old (Context.empty (Context.currentName old)) mempty opens EnvDispatch
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
  Expression tag m => m ()
populateModMap = do
  open <- Juvix.Library.ask @"openMap"
  modM <- get @"modMap"
  currentName <- currentNameSpace
  let curr = pure Context.topLevelName <> currentName
  case open Map.!? curr of
    Nothing ->
      pure ()
    Just opens -> do
      -- TODO ∷
      -- explicts and implicits for now work the same
      -- later they should work as follows
      -- Implicit opens should be superseded by explicit
      -- thus our map should have implciit explicit on
      -- each symbol. Later when we are done, we remove
      -- these markings as they are no longer useful
      -- TODO ∷
      -- should we even append modM to this
      assocNameWithAlias <- concatMapM f opens
      put @"modMap" (modM <> Map.fromList assocNameWithAlias)
      where
        f y = do
          openList <- inScopeNames nameSpace
          maybeAssocNameWithAlias <- traverse addNewSymbol openList
          pure (catMaybes maybeAssocNameWithAlias)
          where
            nameSpace =
              case y of
                Implicit x -> x
                Explicit x -> x
            addNewSymbol x =
              inCurrentModule (NameSymbol.fromSymbol x) >>= \case
                True -> pure Nothing
                -- if the symbol is in the map, then
                -- we don't shadow it....
                -- TODO ∷ later throw an error for
                --        explicit opens that do this
                False -> pure (Just (x, nameSpace))

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
  if  | null cantResolveNow ->
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
                      Just _ -> Left (AmbiguousSymbol x)
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
    setRes l = Set.fromList (notResolved l)
    -- O(n log₁₆(n))
    diff =
      mappend
        (Set.difference (setRes resFull) (setRes resFirst))
        (Set.difference (setRes resFirst) (setRes resFull))

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

-- TODO ∷ add test for firstName
firstName :: NameSymbol.T -> NameSymbol.T
firstName (x :| y : _)
  | Context.topLevelName == x =
    x :| [y]
  | otherwise =
    NameSymbol.fromSymbol x
firstName xs =
  NameSymbol.hd xs
    |> NameSymbol.fromSymbol
