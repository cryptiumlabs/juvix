-- |
-- - This module is responsible for adding the reverse open
--   information to the context, along with the alias map of what
--   symbols get qualified to what module
-- - This module accepts a list of =PreQualified= which talks
--   about
--   1. The explicit module itself
--   2. Any opens this module does
--   3. Any modules defined in this module as to have implicit imports
-- - Currently the most complicated part of this module is the resolve
--   section that creates an =OpenMap=
--   + This code is responsible for taking in all the opens and
--     properly storing them fully qualified.
--   + This has to try to open as much as possible as we could have
--     =open Michelson= =open Prelude=, in which Michelson is inside
--     of prelude so it can't be resolved right away. This way can
--     lead to ambiguities if it does exist so one has to be a bit
--     careful opening in this way!
-- - The other bits of code are stand alone algorithms for filling in
--   the reverse map and the qualification from that point
--   forward.... these are thankfully quite straight forward
module Juvix.Contextify.ToContext.ResolveOpenInfo where

import Control.Lens hiding ((|>))
import qualified Data.HashSet as Set
import qualified Juvix.Core.Common.Context as Context
import qualified Juvix.Core.Common.NameSpace as NameSpace
import qualified Juvix.Core.Common.Open as Open
import Juvix.Library
import qualified Juvix.Library.HashMap as HashMap
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified StmContainers.Map as STM

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

data PreQualified = Pre
  { opens :: [NameSymbol.T],
    implicitInner :: [NameSymbol.T],
    explicitModule :: NameSymbol.T
  }
  deriving (Show, Eq)

data Error
  = UnknownModule NameSymbol.T
  | CantResolveModules [NameSymbol.T]
  | OpenNonModule (Set.HashSet Context.NameSymbol)
  | AmbiguousSymbol Symbol
  | ModuleConflict Symbol [NameSymbol.T]
  | IllegalModuleSwitch Context.NameSymbol
  deriving (Show, Eq)

data Resolve a b c = Res
  { resolved :: [(Context.From (Context.Definition a b c), NameSymbol.T)],
    notResolved :: [NameSymbol.T]
  }
  deriving (Show)

type PureSymbolMap = HashMap.Map Symbol Context.SymbolInfo

type OpenMap = HashMap.T NameSymbol.T [Open NameSymbol.T]

data Open a
  = Implicit a
  | Explicit a
  deriving (Show, Eq, Ord)

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
  resolved <-
    runM $ do
      resolved <- resolve ctx qualifieds
      populateOpens resolved ctx
      createReverseOpenMap resolved ctx
  case resolved of
    Right reverse -> pure (Right ctx {Context.reverseLookup = reverse})
    Left error -> pure (Left error)

--------------------------------------------------------------------------------
-- Functionality operating over a fully resolved open Map
--------------------------------------------------------------------------------

createReverseOpenMap ::
  (HasThrow "left" Error m, MonadIO m) => OpenMap -> Context.T a b c -> m Context.ReverseLookup
createReverseOpenMap open ctx = do
  foldM (resolveReverseOpen ctx) mempty (HashMap.toList open)

populateOpens ::
  (HasThrow "left" Error m, MonadIO m) => OpenMap -> Context.T a b c -> m ()
populateOpens opens ctx = do
  traverse_ (populateOpen ctx) (HashMap.toList opens)

populateOpen ::
  (HasThrow "left" Error m, MonadIO m) =>
  Context.T a b c ->
  (NameSymbol.T, [Open NameSymbol.T]) ->
  m ()
populateOpen ctx (explicitModule, opens) = do
  inScopeNames <- grabInScopeNames ctx explicitModule
  symbolMap <- grabQualifiedMap ctx explicitModule
  pureM <- pureHashMap inScopeNames
  liftIO (hashMaptoSTMMap (convertValuesToSymbolInfo pureM) symbolMap)
  where
    f inScopeNames map a =
      let (sym, impExp) =
            case a of
              Implicit x -> (x, Open.Implicit)
              Explicit x -> (x, Open.Explicit)
          NameSpace.List {publicL} = grabList sym ctx
          --
          updateSymbol _ Nothing =
            pure $ Just (impExp, sym)
          updateSymbol _ (Just (Open.Implicit, _))
            | Open.Implicit /= impExp =
              pure $ Just (impExp, sym)
          updateSymbol _ same@(Just (Open.Explicit, _))
            | Open.Implicit == impExp =
              pure same
          updateSymbol key (Just (_, sym')) =
            throw @"left" (ModuleConflict key [sym, sym'])
          --
          checkInScopeUp sym val =
            case NameSpace.lookupInternal sym inScopeNames of
              Just __ -> pure Nothing
              Nothing -> updateSymbol sym val
       in foldM
            (\map x -> HashMap.alterF (checkInScopeUp x) x map)
            map
            (fmap fst publicL)
    convertValuesToSymbolInfo =
      HashMap.map (\(_, mod) -> Context.SymInfo {used = Context.NotUsed, mod})
    pureHashMap inScopeNames = foldM (f inScopeNames) mempty opens

resolveReverseOpen ::
  (HasThrow "left" Error m, MonadIO m) =>
  Context.T a b c ->
  Context.ReverseLookup ->
  (NameSymbol.T, [Open NameSymbol.T]) ->
  m Context.ReverseLookup
resolveReverseOpen ctx reverseLookup (explicitModule, opens) = do
  symbolMap <- grabQualifiedMap ctx explicitModule
  let updateRevLook impExplict existing =
        let whousesInfo =
              Context.Who {impExplict, modName = explicitModule, symbolMap}
         in case existing of
              Nothing -> Just [whousesInfo]
              Just es -> Just (whousesInfo : es)
      alterUpdate (Implicit a) = HashMap.alter (updateRevLook Open.Implicit) a
      alterUpdate (Explicit a) = HashMap.alter (updateRevLook Open.Explicit) a
  pure (foldr alterUpdate reverseLookup opens)

hashMaptoSTMMap ::
  (Eq k, Hashable k) => HashMap.HashMap k v -> STM.Map k v -> IO ()
hashMaptoSTMMap pureMap stmMap =
  traverse_
    (\(k, v) -> atomically $ STM.insert v k stmMap)
    (HashMap.toList pureMap)

----------------------------------------
-- New Helpers
----------------------------------------

grabInScopeNames ::
  HasThrow "left" Error m =>
  Context.T a b c ->
  NameSymbol.T ->
  m (NameSpace.T (Context.Definition a b c))
grabInScopeNames ctx name =
  case Context.extractValue <$> Context.lookup name ctx of
    Just (Context.Record rec') ->
      pure (rec' ^. Context.contents)
    _ ->
      throw @"left" (UnknownModule (Context.qualifyName name ctx))

grabQualifiedMap ::
  HasThrow "left" Error m => Context.T a b c -> NameSymbol.T -> m Context.SymbolMap
grabQualifiedMap ctx name =
  case Context.extractValue <$> Context.lookup name ctx of
    Just (Context.Record rec') ->
      pure (rec' ^. Context.qualifiedMap)
    _ ->
      throw @"left" (UnknownModule (Context.qualifyName name ctx))

-- | @removeRedundantQualifieds@ takes a qualified and removes any
-- redundant imports it may contain
-- removeRedundantQualifieds Pre {opens, implicitInner, explicitModule} = undefined

--------------------------------------------------------------------------------
-- Code for generating the fully realized OpenMap
--------------------------------------------------------------------------------

-- We do some repeat work here

-- This section is messy and was copied from a previous iteration The
-- algorithm below tries to resolve all modules fully and gives an
-- explicitly qualified mapping.

-- TODO ∷ how do implicit opens and explicit interact, who wins?  Since
-- this generates a list we can probably just filter and ste out the
-- differences

-- | @resolve@ takes a context and a list of open modules and the modules
-- they are open in (the module itself, and sub modules, which the opens are
-- implicit), and fully resolves the opens to their full name.
-- for example @open Prelude@ in the module Londo translates to
-- @resolve ctx PreQualified {opens = [Prelude]}@
-- == @Right (fromList [(TopLevel :| [Londo],[Explicit (TopLevel :| [Prelude])])])@
resolve ::
  (HasThrow "left" Error m, MonadIO m) => Context.T a b c -> [PreQualified] -> m OpenMap
resolve ctx = foldM (resolveSingle ctx) mempty

resolveSingle ::
  (HasThrow "left" Error m, MonadIO m) =>
  Context.T a b c ->
  OpenMap ->
  PreQualified ->
  m OpenMap
resolveSingle ctx openMap Pre {opens, implicitInner, explicitModule} = do
  switched <- liftIO $ Context.switchNameSpace explicitModule ctx
  case switched of
    Left (Context.VariableShared err) ->
      throw @"left" (IllegalModuleSwitch err)
    Right ctx -> do
      resolved <- liftEither (pathsCanBeResolved ctx opens)
      qualifiedNames <- liftEither (resolveLoop ctx mempty resolved)
      openMap
        |> HashMap.insert explicitModule (fmap Explicit qualifiedNames)
        |> ( \map ->
               foldr
                 (\mod -> HashMap.insert mod (fmap Implicit qualifiedNames))
                 map
                 implicitInner
           )
        |> pure

-- this goes off after the checks have passed regarding if the paths
-- are even possible to resolve
resolveLoop ::
  Context.T a b c ->
  HashMap.Map Symbol NameSymbol.T ->
  Resolve a b c ->
  Either Error [NameSymbol.T]
resolveLoop ctx map Res {resolved, notResolved = cantResolveNow} = do
  map <- foldM addToModMap map fullyQualifyRes
  --
  let newResolve = resolveWhatWeCan ctx (qualifyCant map <$> cantResolveNow)
  --
  if
      | null cantResolveNow ->
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
        Context.Record Context.Rec {recordContents} ->
          let NameSpace.List {publicL} = NameSpace.toList recordContents
           in foldlM
                ( \map x ->
                    case map HashMap.!? x of
                      -- this means we already have opened this in another
                      -- module
                      Just _ -> Left (AmbiguousSymbol x)
                      -- if it's already in scope from below or above us
                      -- don't add to the remapping
                      Nothing ->
                        case Context.lookup (pure x) ctx of
                          Just _ -> Right map
                          Nothing -> Right (HashMap.insert x sym map)
                )
                map
                (fmap fst publicL)
        -- should be updated when we can open expressions
        _ ->
          Left (UnknownModule sym)
    qualifyCant newMap term =
      case newMap HashMap.!? NameSymbol.hd term of
        Nothing ->
          term
        Just x ->
          x <> term

-- Since Locals and top level beats opens, we can determine from the
-- start that any module which tries to open a nested path fails,
-- yet any previous part succeeds, that an illegal open is happening,
-- and we can error out immediately

liftEither :: HasThrow "left" e m => Either e a -> m a
liftEither (Left le) = throw @"left" le
liftEither (Right x) = pure x

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

----------------------------------------
-- Helper functions for the instances
----------------------------------------

grabList ::
  NameSymbol.T -> Context.T a b c -> NameSpace.List (Context.Definition a b c)
grabList name ctx =
  case Context.extractValue <$> Context.lookup name ctx of
    Just (Context.Record Context.Rec {recordContents}) ->
      NameSpace.toList recordContents
    Just _ ->
      NameSpace.List [] []
    Nothing ->
      NameSpace.List [] []
