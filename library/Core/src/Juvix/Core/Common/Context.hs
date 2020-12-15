-- |
-- - Serves as the context for lower level programs of the =Juvix=
--   Programming Language
-- - This is parameterized per phase which may store the type and
--   term in slightly different ways
module Juvix.Core.Common.Context
  ( module Juvix.Core.Common.Context.Types,
    module Juvix.Core.Common.Context.Precedence,
    -- leave the entire module for now, so lenses can be exported
    module Juvix.Core.Common.Context,
    Group,
    Entry (..),
    recGroups,
  )
where

import Control.Lens hiding ((|>))
import Juvix.Core.Common.Context.Precedence
import Juvix.Core.Common.Context.RecGroups
import Juvix.Core.Common.Context.Types
import qualified Juvix.Core.Common.NameSpace as NameSpace
import qualified Juvix.Core.Common.Open as Open
import Juvix.Library hiding (modify)
import qualified Juvix.Library as Lib
import qualified Juvix.Library.HashMap as HashMap
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified StmContainers.Map as STM
import Prelude (error)

--------------------------------------------------------------------------------
-- In lieu of not being able to export namespaces
--------------------------------------------------------------------------------
type NameSymbol = NameSymbol.T

nameSymbolToSymbol :: NameSymbol.T -> Symbol
nameSymbolToSymbol = NameSymbol.toSymbol

nameSymbolFromSymbol :: Symbol -> NameSymbol.T
nameSymbolFromSymbol = NameSymbol.fromSymbol

--------------------------------------------------------------------------------
-- Special Names
--------------------------------------------------------------------------------

topLevelName :: IsString p => p
topLevelName = "TopLevel"

--------------------------------------------------------------------------------
-- Body
--------------------------------------------------------------------------------

empty :: NameSymbol.T -> IO (T term ty sumRep)
empty sym = do
  empty <- atomically fullyEmpty
  res <- addPathWithValue (pure topLevelName <> sym') CurrentNameSpace empty
  case res of
    Lib.Left _ -> error "impossible"
    Lib.Right x -> pure x
  where
    fullyEmpty = do
      emptyReverseLookup <- STM.new
      pure $
        T
          { currentNameSpace = NameSpace.empty,
            currentName = sym',
            topLevelMap = HashMap.empty,
            reverseLookup = emptyReverseLookup
          }
    sym' = removeTopName sym

qualifyName :: NameSymbol.T -> T term ty sumRep -> NameSymbol.T
qualifyName sym T {currentName} = currentName <> sym

--------------------------------------------------------------------------------
-- Functions on the Current NameSpace
--------------------------------------------------------------------------------

lookupCurrent ::
  NameSymbol.T -> T term ty sumRep -> Maybe (NameSpace.From (Definition term ty sumRep))
lookupCurrent =
  lookupGen (\_ currentLookup -> currentLookup)

-- TODO ∷ Maybe change
-- By default add adds it to the public map by default!
add ::
  NameSpace.From Symbol ->
  Definition term ty sumRep ->
  T term ty sumRep ->
  T term ty sumRep
add sy term t =
  t {currentNameSpace = NameSpace.insert sy term (currentNameSpace t)}

remove ::
  NameSpace.From Symbol -> T term ty sumRep -> T term ty sumRep
remove sy t = t {currentNameSpace = NameSpace.remove sy (currentNameSpace t)}

publicNames :: T term ty sumRep -> [Symbol]
publicNames T {currentNameSpace} =
  let NameSpace.List {publicL} = NameSpace.toList currentNameSpace
   in fst <$> publicL

toList :: T term ty sumRep -> NameSpace.List (Definition term ty sumRep)
toList T {currentNameSpace} = NameSpace.toList currentNameSpace

topList :: T term ty sumRep -> [(Symbol, Definition term ty sumRep)]
topList T {topLevelMap} = HashMap.toList topLevelMap

--------------------------------------------------------------------------------
-- Global Functions
--------------------------------------------------------------------------------

-- we lose some type information here... we should probably reserve it somehow

-- | switchNameSpace works like a mixture of defpackage and in-package from CL
-- creating a namespace if not currently there, and switching to it
-- this function may fail if a path is given like `Foo.Bar.x.f` where x
-- is a non record.
-- This function also keeps the invariant that there is only one CurrentNameSpace
-- tag
switchNameSpace ::
  NameSymbol.T -> T term ty sumRep -> IO (Either PathError (T term ty sumRep))
switchNameSpace newNameSpace t@T {currentName}
  | removeTopName newNameSpace == removeTopName currentName =
    pure (Lib.Right t)
  | otherwise = do
    let addCurrentName t startingContents newCurrName =
          (addGlobal' t)
            { currentName = removeTopName newCurrName,
              currentNameSpace = startingContents
            }
        -- addGlobal' adds the current name space back to the environment map
        addGlobal' t@T {currentNameSpace} =
          -- we have to add top to it, or else if it's a single symbol, then
          -- it'll be added to itself, which is bad!
          addGlobal
            (addTopNameToSngle currentName)
            (Record $ Rec currentNameSpace Nothing undefined undefined)
            t
        addCurrent = addGlobal newNameSpace CurrentNameSpace
        qualifyName =
          currentName <> newNameSpace
    ret <- addPathWithValue newNameSpace CurrentNameSpace t
    pure $ case ret of
      -- In this path the CurrentNameSpace is already added
      Lib.Right t ->
        -- check if we have to qualify Name
        case t !? newNameSpace of
          Just (Current _) ->
            Lib.Right (addCurrentName t NameSpace.empty qualifyName)
          _ ->
            Lib.Right (addCurrentName t NameSpace.empty newNameSpace)
      -- the namespace may already exist
      -- in this path CurrentNameSpace has not been added, we MUST add it
      Lib.Left er ->
        -- TODO ∷ refactor with resolveName
        -- how do we add the namespace back to the private area!?
        case t !? newNameSpace of
          Just (Current (NameSpace.Pub (Record record))) ->
            Lib.Right
              (addCurrent (addCurrentName t (record ^. contents) qualifyName))
          Just (Current (NameSpace.Priv (Record record))) ->
            Lib.Right
              (addCurrent (addCurrentName t (record ^. contents) qualifyName))
          Just (Outside (Record record))
            -- figure out if we contain what we are looking for!
            | NameSymbol.prefixOf (removeTopName newNameSpace) currentName ->
              -- if so update it!
              case addGlobal' t !? newNameSpace of
                Just (Outside (Record record)) ->
                  Lib.Right
                    (addCurrent (addCurrentName t (record ^. contents) newNameSpace))
                _ -> error "doesn't happen"
            | otherwise ->
              Lib.Right
                (addCurrent (addCurrentName t (record ^. contents) newNameSpace))
          Nothing -> Lib.Left er
          Just __ -> Lib.Left er

addTopNameToSngle :: IsString a => NonEmpty a -> NonEmpty a
addTopNameToSngle (x :| []) = topLevelName :| [x]
addTopNameToSngle xs = xs

removeTopName :: (Eq a, IsString a) => NonEmpty a -> NonEmpty a
removeTopName (top :| x : xs)
  | topLevelName == top = x :| xs
removeTopName (top :| [])
  | top == topLevelName = "" :| []
removeTopName xs = xs

lookup ::
  NameSymbol.T -> T term ty sumRep -> Maybe (From (Definition term ty sumRep))
lookup key t@T {topLevelMap} =
  let f x currentLookup =
        fmap Current currentLookup <|> fmap Outside (HashMap.lookup x topLevelMap)
   in lookupGen f key t

(!?) ::
  T term ty sumRep -> NameSymbol.T -> Maybe (From (Definition term ty sumRep))
(!?) = flip lookup

modifyGlobal ::
  NameSymbol.T ->
  (Maybe (Definition term ty sumRep) -> Definition term ty sumRep) ->
  T term ty sumRep ->
  T term ty sumRep
modifyGlobal sym g t =
  case modifySpace f sym t of
    Just tt -> tt
    Nothing -> t
  where
    f (Final x) =
      UpdateNow (g x)
    f (Continue (Just (Record record))) =
      GoOn record
    f (Continue _) =
      Abort

addGlobal ::
  NameSymbol.T ->
  Definition term ty sumRep ->
  T term ty sumRep ->
  T term ty sumRep
addGlobal sym def t =
  case modifySpace f sym t of
    Just tt -> tt
    Nothing -> t
  where
    f (Final _) =
      UpdateNow def
    f (Continue (Just (Record record))) =
      GoOn record
    f (Continue _) =
      Abort

addPathWithValue ::
  NameSymbol.T ->
  Definition term ty sumRep ->
  T term ty sumRep ->
  IO (Either PathError (T term ty sumRep))
addPathWithValue sym def t = do
  ret <- modifySpaceImp f sym t
  case ret of
    Just tt -> pure (Lib.Right tt)
    Nothing -> pure (Lib.Left (VariableShared sym))
  where
    f (Final Nothing) = pure (UpdateNow def)
    f (Final (Just _)) = pure Abort
    f (Continue Nothing) =
      atomically STM.new
        >>| GoOn . Rec NameSpace.empty Nothing []
    f (Continue (Just (Record record))) =
      pure (GoOn record)
    f (Continue _) =
      pure Abort

removeNameSpace :: NameSymbol -> T term ty sumRep -> T term ty sumRep
removeNameSpace sym t =
  case modifySpace f sym t of
    Just tt -> tt
    Nothing -> t
  where
    f (Final (Just _)) =
      RemoveNow
    f (Final Nothing) =
      Abort
    f (Continue (Just (Record record))) =
      GoOn record
    f (Continue Nothing) =
      Abort
    f (Continue (Just _)) =
      Abort

removeTop :: Symbol -> T term ty sumRep -> T term ty sumRep
removeTop sym t@T {topLevelMap} =
  t {topLevelMap = HashMap.delete sym topLevelMap}

-------------------------------------------------------------------------------
-- Functions on From
-------------------------------------------------------------------------------

extractValue :: From a -> a
extractValue (Outside a) = a
extractValue (Current c) = NameSpace.extractValue c

-------------------------------------------------------------------------------
-- Functions on Information
-------------------------------------------------------------------------------
precedenceOf :: Foldable t => t Information -> Maybe Precedence
precedenceOf = fmap (\(Prec p) -> p) . find f
  where
    f (Prec _) = True

-------------------------------------------------------------------------------
-- Generalized Helpers
-------------------------------------------------------------------------------

----------------------------------------
-- Types for Generalized Helpers
----------------------------------------

data Stage b
  = -- | 'Final' signifies the last symbol that we
    -- pursue in updating a structure
    Final b
  | -- | 'Continue' signifies that there are parts of
    -- the namespace that we can still continue down
    Continue b

data Return term ty sumRep
  = -- | 'GoOn' signifies that we should continue
    -- going down records
    GoOn (Record term ty sumRep)
  | -- | 'Abort' signifies that we should cancel
    -- the changes on the map and
    Abort
  | -- | 'UpdateNow' signifies that we should
    -- update the context with the current value
    UpdateNow (Definition term ty sumRep)
  | -- | 'RemoveNow' signifies that we show remove
    -- the definition at this level
    RemoveNow

----------------------------------------
-- Type Class for Genralized Helpers
----------------------------------------

-- Type class for removing repeat code
class MapSym m where
  lookup' :: Symbol -> m a -> Maybe a
  remove' :: Symbol -> m a -> m a
  insert' :: Symbol -> a -> m a -> m a

instance MapSym (HashMap.T Symbol) where
  lookup' = HashMap.lookup
  remove' = HashMap.delete
  insert' = HashMap.insert

instance MapSym NameSpace.T where
  lookup' = NameSpace.lookup
  remove' = NameSpace.removePublic
  insert' x = NameSpace.insert (NameSpace.Pub x)

newtype PrivNameSpace v = Priv {unPriv :: NameSpace.T v}

instance MapSym PrivNameSpace where
  lookup' sym = NameSpace.lookup sym . unPriv
  remove' sym = Priv . NameSpace.removePrivate sym . unPriv
  insert' sym def = Priv . NameSpace.insert (NameSpace.Priv sym) def . unPriv

modifySpace ::
  (Stage (Maybe (Definition term ty sumRep)) -> Return term ty sumRep) ->
  NameSymbol.T ->
  T term ty sumRep ->
  Maybe (T term ty sumRep)
modifySpace f symbol t = runIdentity (modifySpaceImp (Identity . f) symbol t)

modifySpaceImp ::
  Monad m =>
  ( Stage (Maybe (Definition term ty sumRep)) ->
    m (Return term ty sumRep)
  ) ->
  NameSymbol.T ->
  T term ty sumRep ->
  m (Maybe (T term ty sumRep))
modifySpaceImp f (s :| ymbol) t@T {currentNameSpace, currentName, topLevelMap} =
  -- check the top level first, as per the lookup rules
  case NameSpace.lookupInternal s currentNameSpace of
    Just (NameSpace.Pub _) -> do
      ret <- recurseImp f (s :| ymbol) currentNameSpace
      pure (updateCurr t <$> ret)
    Just (NameSpace.Priv _) -> do
      ret <- recurseImp f (s :| ymbol) (Priv currentNameSpace)
      pure (updateCurr t . unPriv <$> ret)
    -- Check the current namespace, since it's not at the top level
    Nothing ->
      case NameSymbol.takePrefixOf currentName (removeTopName (s :| ymbol)) of
        Just subPath -> do
          ret <- recurseImp f subPath currentNameSpace
          pure (updateCurr t <$> ret)
        Nothing ->
          -- we do one more match as we have to make sure
          -- if we are adding foo we add it to the right place
          case ymbol of
            [] -> do
              ret <- recurseImp f (s :| ymbol) currentNameSpace
              pure (updateCurr t <$> ret)
            (newS : newYmbol) ->
              -- if s is a top level then we should try on without it
              if  | s == topLevelName -> do
                    ret <- recurseImp f (newS :| newYmbol) topLevelMap
                    pure (updateTopLevel t <$> ret)
                  -- s is not a top level just keep continuing
                  | otherwise -> do
                    ret <- recurseImp f (s :| ymbol) topLevelMap
                    pure (updateTopLevel t <$> ret)
  where
    updateCurr t newCurrent =
      t {currentNameSpace = newCurrent}
    updateTopLevel t newTop =
      t {topLevelMap = newTop}

recurseImp ::
  (MapSym map, Monad m) =>
  ( Stage (Maybe (Definition term ty sumRep)) ->
    m (Return term ty sumRep)
  ) ->
  NameSymbol.T ->
  map (Definition term ty sumRep) ->
  m (Maybe (map (Definition term ty sumRep)))
recurseImp f (x :| y : xs) cont = do
  ret <- f (Continue (lookup' x cont))
  case ret of
    GoOn record -> do
      recursed <- recurseImp f (y :| xs) (record ^. contents)
      let g newRecord =
            insert' x (record |> set contents newRecord |> Record) cont
       in pure (g <$> recursed)
    Abort ->
      pure Nothing
    RemoveNow ->
      pure (Just (remove' x cont))
    UpdateNow newRecord ->
      pure (Just (insert' x newRecord cont))
recurseImp f (x :| []) cont = do
  ret <- f (Final (lookup' x cont))
  case ret of
    UpdateNow return ->
      pure (Just (insert' x return cont))
    RemoveNow ->
      pure (Just (remove' x cont))
    -- GoOn makes no sense here, so act as an Abort
    GoOn {} -> pure Nothing
    Abort -> pure Nothing

-- couldn't figure out how to fold lenses
-- once we figure out how to do a fold like
-- foldr (\x y -> x . contents . T  . y) identity brokenKey
-- replace the recursive function with that

-- TODO ∷ add something like
-- checkGlobal
--   | NameSymbol.subsetOf currentName nameSymb

-- eventually to check if we are referencing an inner module via the top
-- This will break code where you've added local

lookupGen ::
  Traversable t =>
  ( Symbol ->
    Maybe (NameSpace.From (Definition term ty sumRep)) ->
    Maybe (t (Definition term ty sumRep))
  ) ->
  NameSymbol.T ->
  Cont (Definition term ty sumRep) ->
  Maybe (t (Definition term ty sumRep))
lookupGen extraLookup nameSymb T {currentNameSpace} =
  let recurse _ Nothing =
        Nothing
      recurse [] x =
        x
      recurse (x : xs) (Just (Record record)) =
        recurse xs (NameSpace.lookup x (record ^. contents))
      -- This can only happen when we hit from the global
      -- a precondition is that the current module
      -- will never have a currentNamespace inside
      recurse (x : xs) (Just CurrentNameSpace) =
        recurse xs (NameSpace.lookup x currentNameSpace)
      recurse (_ : _) _ =
        Nothing
      first (x :| xs) =
        NameSpace.lookupInternal x currentNameSpace
          |> second (x :| xs)
      second (x :| xs) looked =
        extraLookup x looked
          |> \case
            Just x -> traverse (recurse xs . Just) x
            Nothing -> Nothing
   in case nameSymb of
        -- we skip the local lookup if we get a top level
        top :| x : xs
          | topLevelName == top -> second (x :| xs) Nothing
        top :| []
          | topLevelName == top -> Nothing
        x :| xs -> first (x :| xs)

-- TODO/ISSUE ∷ what if we resolve a private module
-- the path and lookup doesn't make sense much
-- we need to change namesymbol to something along
-- the lines of every symbol saying if it's public
-- or private
resolveName ::
  T a b c ->
  (From (Definition a b c), NameSymbol.T) ->
  (Definition a b c, NameSymbol.T)
resolveName ctx (def, name) =
  case def of
    -- TODO ∷ update this case
    Current (NameSpace.Priv x) ->
      (x, fullyQualified)
    Current (NameSpace.Pub x) ->
      (x, fullyQualified)
    Outside x ->
      (x, nameAlreadyFully)
  where
    nameAlreadyFully =
      pure topLevelName <> removeTopName name
    fullyQualified =
      pure topLevelName <> currentName ctx <> name

-- | Traverses a whole context by performing an action on each recursive group.
-- The groups are passed in dependency order but the order of elements within
-- each group is arbitrary.
traverseContext ::
  (Applicative f, Monoid t) =>
  -- | process one recursive group
  (Group a b c -> f t) ->
  T a b c ->
  f t
traverseContext f = foldMapA f . recGroups

-- | Same as 'traverseContext', but the groups are split up into single
-- definitions.
traverseContext1 ::
  (Monoid t, Applicative f) =>
  -- | process one definition
  (NameSymbol.T -> Definition a b c -> f t) ->
  T a b c ->
  f t
traverseContext1 = traverseContext . foldMapA . onEntry
  where
    onEntry f (Entry {name, def}) = f name def
