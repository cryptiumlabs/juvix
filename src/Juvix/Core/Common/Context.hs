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
  deriving (Show)

type T term ty sumRep = Cont (Definition term ty sumRep)

data From b
  = Current (NameSpace.From b)
  | Outside b
  deriving (Show, Functor, Traversable, Foldable)

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
  deriving (Show, Generic)

-- not using lenses anymore but leaving this here anyway
makeLensesWith camelCaseFields ''Definition

data PathError
  = VariableShared NameSymbol.T
  deriving (Show, Eq)

--------------------------------------------------------------------------------
-- In Lu of not being able to export namespaces
--------------------------------------------------------------------------------
type NameSymbol = NameSymbol.T

nameSymbolToSymbol :: NameSymbol.T -> Symbol
nameSymbolToSymbol = NameSymbol.toSymbol

nameSymbolFromSymbol :: Symbol -> NameSymbol.T
nameSymbolFromSymbol = NameSymbol.fromSymbol

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

--------------------------------------------------------------------------------
-- Global Functions
--------------------------------------------------------------------------------
-- we lose some type information here... we should probably reserve it somehow
switchNameSpace ::
  NameSymbol.T -> T term ty sumRep -> Either PathError (T term ty sumRep)
switchNameSpace newNameSpace t@T {currentName} =
  let addCurrentName t@T {currentNameSpace} startingContents =
        (addGlobal currentName (Record currentNameSpace Nothing) t)
          { currentName = newNameSpace,
            currentNameSpace = startingContents
          }
   in case addPathWithValue newNameSpace CurrentNameSpace t of
        Lib.Right t ->
          Lib.Right (addCurrentName t NameSpace.empty)
        -- the namespace may already exist
        Lib.Left er ->
          -- how do we add the namespace back to the private area!?
          case t !? newNameSpace of
            Just (Current (NameSpace.Pub (Record def _))) ->
              Lib.Right (addCurrentName t def)
            Just (Current (NameSpace.Priv (Record def _))) ->
              Lib.Right (addCurrentName t def)
            Just (Outside (Record def _)) ->
              Lib.Right (addCurrentName t def)
            Nothing -> Lib.Left er
            Just __ -> Lib.Left er

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

-------------------------------------------------------------------------------
-- Generalized Helpers
--------------------------------------------------------------------------------

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
      updateCurr t <$> recurse (s :| ymbol) currentNameSpace
    Just (NameSpace.Priv _) ->
      updateCurr t <$> recursePriv (s :| ymbol) currentNameSpace
    Nothing ->
      case NameSymbol.takeSubSetOf currentName (s :| ymbol) of
        Just subPath ->
          updateCurr t <$> recurse subPath currentNameSpace
        Nothing ->
          updateTopLevel t <$> hashRecurse (s :| ymbol)
  where
    updateCurr t newCurrent =
      t {currentNameSpace = newCurrent}
    updateTopLevel t newTop =
      t {topLevelMap = newTop}
    recurse (x :| y : xs) cont =
      case f (Continue (NameSpace.lookup x cont)) of
        GoOn (OnRecord nameSpace ty) ->
          let f newRecord =
                NameSpace.insert (NameSpace.Pub x) (Record newRecord ty) cont
           in fmap f (recurse (y :| xs) nameSpace)
        Abort ->
          Nothing
        RemoveNow ->
          Just (NameSpace.remove (NameSpace.Pub x) cont)
        UpdateNow newRecord ->
          Just (NameSpace.insert (NameSpace.Pub x) newRecord cont)
    recurse (x :| []) cont =
      case f (Final (NameSpace.lookup x cont)) of
        UpdateNow return ->
          Just (NameSpace.insert (NameSpace.Pub x) return cont)
        RemoveNow ->
          Just (NameSpace.remove (NameSpace.Pub x) cont)
        -- GoOn makes no sense here, so act as an Abort
        GoOn {} -> Nothing
        Abort -> Nothing
    -- this function is the same copy and pasted, but deals with hash insert and
    -- lookup instead... figure out how to make it infer the type better later
    -- 2 variables vs 1 is annoying!
    hashRecurse (x :| y : xs) =
      case f (Continue (HashMap.lookup x topLevelMap)) of
        GoOn (OnRecord nameSpace ty) ->
          let f newRecord =
                HashMap.insert x (Record newRecord ty) topLevelMap
           in fmap f (recurse (y :| xs) nameSpace)
        Abort ->
          Nothing
        RemoveNow ->
          Just (HashMap.delete x topLevelMap)
        UpdateNow newRecord ->
          Just (HashMap.insert x newRecord topLevelMap)
    hashRecurse (x :| []) =
      case f (Final (HashMap.lookup x topLevelMap)) of
        UpdateNow return ->
          Just (HashMap.insert x return topLevelMap)
        RemoveNow ->
          Just (HashMap.delete x topLevelMap)
        -- GoOn makes no sense here, so act as an Abort
        GoOn {} -> Nothing
        Abort -> Nothing
    -- More repeat code, but this time we do it on the private
    -- we don't abstract it, as Ι think it'll subtract readability
    -- from the public one
    recursePriv (x :| y : xs) cont =
      case f (Continue (NameSpace.lookupPrivate x cont)) of
        GoOn (OnRecord nameSpace ty) ->
          let f newRecord =
                NameSpace.insert (NameSpace.Priv x) (Record newRecord ty) cont
           in fmap f (recurse (y :| xs) nameSpace)
        RemoveNow ->
          Just (NameSpace.remove (NameSpace.Priv x) cont)
        Abort ->
          Nothing
        UpdateNow newRecord ->
          Just (NameSpace.insert (NameSpace.Priv x) newRecord cont)
    recursePriv (x :| []) cont =
      case f (Final (NameSpace.lookupPrivate x cont)) of
        UpdateNow return ->
          Just (NameSpace.insert (NameSpace.Priv x) return cont)
        RemoveNow ->
          Just (NameSpace.remove (NameSpace.Priv x) cont)
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

-- eventually to check if we are referncing an inner module via the top
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
   in case nameSymb of
        x :| xs ->
          NameSpace.lookupInternal x currentNameSpace
            |> extraLookup x
            |> \case
              Just x -> traverse (recurse xs . Just) x
              Nothing -> Nothing
-- TODO :: change this to an include
-- open :: Symbol -> T term ty sumRep -> T term ty sumRep
-- open key (T map) =
--   case lookup key (T map) of
--     Just (Record (T contents) _) ->
--       -- Union takes the first if there is a conflict
--       T (HashMap.union contents map)
--     Just _ -> T map
--     Nothing -> T map
