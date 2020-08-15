{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- - Serves as the context for lower level programs of the =Juvix=
--   Programming Language
-- - This is parameterized per phase which may store the type and
--   term in slightly different ways
module Juvix.Core.Common.Context
  ( module Juvix.Core.Common.Context.Precedence,
    -- leave the entire module for now, so lenses can be exported
    module Juvix.Core.Common.Context,
  )
where

import Control.Lens hiding ((|>))
import Juvix.Core.Common.Context.Precedence
import qualified Juvix.Core.Common.NameSpace as NameSpace
import qualified Juvix.Core.Common.NameSymbol as NameSymbol
import qualified Juvix.Core.Usage as Usage
import Juvix.Library hiding (modify)
import qualified Juvix.Library as Lib
import qualified Juvix.Library.HashMap as HashMap
import Prelude (error)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

data Cont b
  = T
      { currentNameSpace :: NameSpace.T b,
        currentName :: NameSymbol.T,
        topLevelMap :: HashMap.T Symbol b
      }
  deriving (Show, Eq)

type T term ty sumRep = Cont (Definition term ty sumRep)

-- | From constitutes where the value we are looking up comes from
-- Does it come from the Current name space, or does it come from some
-- name space from the global map
data From b
  = Current (NameSpace.From b)
  | Outside b
  deriving (Show, Functor, Traversable, Foldable, Eq)

-- TODO :: make known records that are already turned into core
-- this will just emit the proper names we need, not any terms to translate
-- once we hit core, we can then populate it with the actual forms
data Definition term ty sumRep
  = Def
      { definitionUsage :: Maybe Usage.T,
        definitionMTy :: Maybe ty,
        definitionTerm :: term,
        precedence :: Precedence
      }
  | Record
      { definitionContents :: NameSpace.T (Definition term ty sumRep),
        -- Maybe as I'm not sure what to put here for now
        definitionMTy :: Maybe ty
      }
  | TypeDeclar
      { definitionRepr :: sumRep
      }
  | Unknown
      { definitionMTy :: Maybe ty
      }
  | -- Signifies that this path is the current module, and that
    -- we should search the currentNameSpace from here
    CurrentNameSpace
  deriving (Show, Generic, Eq)

-- not using lenses anymore but leaving this here anyway
makeLensesWith camelCaseFields ''Definition

data PathError
  = VariableShared NameSymbol.T
  deriving (Show, Eq)

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

empty :: NameSymbol.T -> T term ty sumRep
empty sym =
  case addPathWithValue sym CurrentNameSpace fullyEmpty of
    Lib.Left _ -> error "impossible"
    Lib.Right x -> x
  where
    fullyEmpty =
      ( T
          { currentNameSpace = NameSpace.empty,
            currentName = sym,
            topLevelMap = HashMap.empty
          }
      )

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
  NameSymbol.T -> T term ty sumRep -> Either PathError (T term ty sumRep)
switchNameSpace newNameSpace t@T {currentName}
  | removeTopName newNameSpace == removeTopName currentName = Lib.Right t
  | otherwise =
    let addCurrentName t startingContents newCurrName =
          (addGlobal' t)
            { currentName = removeTopName newCurrName,
              currentNameSpace = startingContents
            }
        addGlobal' t@T {currentNameSpace} =
          -- we have to add top to it, or else if it's a single symbol, then
          -- it'll be added to itself, which is bad!
          addGlobal
            (addTopNameToSngle currentName)
            (Record currentNameSpace Nothing)
            t
        addCurrent t = addGlobal newNameSpace CurrentNameSpace t
        qualifyName =
          currentName <> newNameSpace
     in case addPathWithValue newNameSpace CurrentNameSpace t of
          Lib.Right t ->
            -- check if we have to qualify Name
            case t !? newNameSpace of
              Just (Current _) ->
                Lib.Right (addCurrentName t NameSpace.empty qualifyName)
              _ ->
                Lib.Right (addCurrentName t NameSpace.empty newNameSpace)
          -- the namespace may already exist
          Lib.Left er ->
            -- how do we add the namespace back to the private area!?
            case t !? newNameSpace of
              Just (Current (NameSpace.Pub (Record def _))) ->
                Lib.Right (addCurrent (addCurrentName t def qualifyName))
              Just (Current (NameSpace.Priv (Record def _))) ->
                Lib.Right (addCurrent (addCurrentName t def qualifyName))
              Just (Outside (Record def _))
                -- figure out if we contain what we are looking for!
                | NameSymbol.prefixOf (removeTopName newNameSpace) currentName ->
                  -- if so update it!
                  case addGlobal' t !? newNameSpace of
                    Just (Outside (Record def _)) ->
                      Lib.Right (addCurrent (addCurrentName t def newNameSpace))
                    _ -> error "doesn't happen"
                | otherwise ->
                  Lib.Right (addCurrent (addCurrentName t def newNameSpace))
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
    f (Continue (Just (Record def ty))) =
      GoOn (OnRecord def ty)
    f (Continue _) =
      Abort

addPathWithValue ::
  NameSymbol.T ->
  Definition term ty sumRep ->
  T term ty sumRep ->
  Either PathError (T term ty sumRep)
addPathWithValue sym def t =
  case modifySpace f sym t of
    Just tt -> Lib.Right tt
    Nothing -> Lib.Left (VariableShared sym)
  where
    f (Final Nothing) = UpdateNow def
    f (Final (Just _)) = Abort
    f (Continue Nothing) =
      GoOn (OnRecord NameSpace.empty Nothing)
    f (Continue (Just (Record def ty))) =
      GoOn (OnRecord def ty)
    f (Continue _) =
      Abort

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
    f (Continue (Just (Record def ty))) =
      GoOn (OnRecord def ty)
    f (Continue Nothing) =
      Abort
    f (Continue (Just _)) =
      Abort

removeTop :: Symbol -> T term ty sumRep -> T term ty sumRep
removeTop sym t@T {topLevelMap} =
  t {topLevelMap = HashMap.delete sym topLevelMap}

-------------------------------------------------------------------------------
-- Functions on From
--------------------------------------------------------------------------------

extractValue :: From a -> a
extractValue (Outside a) = a
extractValue (Current c) = NameSpace.extractValue c

-------------------------------------------------------------------------------
-- Generalized Helpers
--------------------------------------------------------------------------------

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

data Return b ty
  = -- | 'GoOn' signifies that we should continue
    -- going down records
    GoOn (OnRecord b ty)
  | -- | 'Abort' signifies that we should cancel
    -- the changes on the map and
    Abort
  | -- | 'UpdateNow' signifies that we should
    -- update the context with the current value
    UpdateNow b
  | -- | 'RemoveNow' signifies that we show remove
    -- the definition at this level
    RemoveNow

data OnRecord b ty
  = OnRecord (NameSpace.T b) (Maybe ty)

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
  ( Stage (Maybe (Definition term ty sumRep)) ->
    Return (Definition term ty sumRep) ty
  ) ->
  NameSymbol.T ->
  T term ty sumRep ->
  Maybe (T term ty sumRep)
modifySpace f (s :| ymbol) t@T {currentNameSpace, currentName, topLevelMap} =
  case NameSpace.lookupInternal s currentNameSpace of
    Just (NameSpace.Pub _) ->
      updateCurr t <$> recurse f (s :| ymbol) currentNameSpace
    Just (NameSpace.Priv _) ->
      updateCurr t . unPriv <$> recurse f (s :| ymbol) (Priv currentNameSpace)
    Nothing ->
      case NameSymbol.takePrefixOf currentName (s :| ymbol) of
        Just subPath ->
          updateCurr t <$> recurse f subPath currentNameSpace
        Nothing ->
          -- we do one more match as we have to make sure
          -- if we are adding foo we add it to the right place
          case ymbol of
            [] ->
              updateCurr t <$> recurse f (s :| ymbol) currentNameSpace
            (newS : newYmbol) ->
              if  | s == topLevelName ->
                    updateTopLevel t <$> recurse f (newS :| newYmbol) topLevelMap
                  | otherwise ->
                    updateTopLevel t <$> recurse f (s :| ymbol) topLevelMap
  where
    updateCurr t newCurrent =
      t {currentNameSpace = newCurrent}
    updateTopLevel t newTop =
      t {topLevelMap = newTop}

recurse ::
  MapSym m =>
  ( Stage (Maybe (Definition term ty sumRep)) ->
    Return (Definition term ty sumRep) ty
  ) ->
  (NonEmpty Symbol) ->
  m (Definition term ty sumRep) ->
  Maybe (m (Definition term ty sumRep))
recurse f (x :| y : xs) cont =
  case f (Continue (lookup' x cont)) of
    GoOn (OnRecord nameSpace ty) ->
      let g newRecord =
            insert' x (Record newRecord ty) cont
       in fmap g (recurse f (y :| xs) nameSpace)
    Abort ->
      Nothing
    RemoveNow ->
      Just (remove' x cont)
    UpdateNow newRecord ->
      Just (insert' x newRecord cont)
recurse f (x :| []) cont =
  case f (Final (lookup' x cont)) of
    UpdateNow return ->
      Just (insert' x return cont)
    RemoveNow ->
      Just (remove' x cont)
    -- GoOn makes no sense here, so act as an Abort
    GoOn {} -> Nothing
    Abort -> Nothing

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
      recurse (x : xs) (Just (Record namespace _)) =
        recurse xs (NameSpace.lookup x namespace)
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
