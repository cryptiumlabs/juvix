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

empty :: NameSymbol.T -> Cont b
empty sym =
  T
    { currentNameSpace = NameSpace.empty,
      currentName = sym,
      topLevelMap = HashMap.empty
    }

qualifyName :: NameSymbol.T -> T term ty sumRep -> NameSymbol.T
qualifyName sym T {currentName} = currentName <> sym

--------------------------------------------------------------------------------
-- Functions on the Current NameSpace
--------------------------------------------------------------------------------

lookupCurrent ::
  Symbol -> T term ty sumRep -> Maybe (NameSpace.From (Definition term ty sumRep))
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

lookup ::
  Symbol -> T term ty sumRep -> Maybe (From (Definition term ty sumRep))
lookup key t@T {topLevelMap} =
  let f x currentLookup =
        fmap Current currentLookup <|> fmap Outside (HashMap.lookup x topLevelMap)
   in lookupGen f key t

(!?) ::
  T term ty sumRep -> Symbol -> Maybe (From (Definition term ty sumRep))
(!?) = flip lookup

-- Used for add namespace adding and various other purposes
addGlobal ::
  NameSymbol.T ->
  Definition term ty sumRep ->
  T term ty sumRep ->
  T term ty sumRep
addGlobal = undefined

addPathWithValue ::
  NameSymbol.T ->
  Definition term ty sumRep ->
  T term ty sumRep ->
  Either PathError (T term ty sumRep)
addPathWithValue path placeholder T {currentNameSpace, currentName, topLevelMap} =
  -- check if the new path is inside the currentNameSpace
  case NameSymbol.takeSubSetOf currentName path of
    Just (p :| pathAfterCurr) ->
      createPath (p : pathAfterCurr) currentNameSpace placeholder
        |> handleMaybe (`create` topLevelMap)
    Nothing ->
      case HashMap.lookup p topLevelMap of
        Just (Record def ty) ->
          continuePathTopLevel def ty
        Nothing -> continuePathTopLevel NameSpace.empty Nothing
        Just __ -> Lib.Left (VariableShared path)
  where
    (p :| path') = path
    create curr top =
      T {currentNameSpace = curr, currentName, topLevelMap = top}
    --
    handleMaybe :: (x -> y) -> Maybe x -> Either PathError y
    handleMaybe _ Nothing =
      Lib.Left (VariableShared path)
    handleMaybe f (Just x) =
      Lib.Right (f x)
    --
    insertRecordTopLevel typ ins =
      HashMap.insert p (Record ins typ) topLevelMap
    --
    continuePathTopLevel next typ =
      createPath path' next placeholder
        |> fmap (insertRecordTopLevel typ)
        |> handleMaybe (create currentNameSpace)

-- this function does not remove the current name space from the toplevel map
-- as we may be able to reference it from the top
switchNameSpace :: NameSymbol.T -> T term ty sumRep -> T term ty sumRep
switchNameSpace newNameSpace T {currentNameSpace, currentName, topLevelMap} = undefined

------------------------------------------------------------
-- Helpers for create path
------------------------------------------------------------

-- this code duplicates some logic with addPathWithValue due
-- to signature annoyances between hashtables and NameSpaces
createPath ::
  [Symbol] ->
  NameSpace.T (Definition term ty sumRep) ->
  Definition term ty sumRep ->
  Maybe (NameSpace.T (Definition term ty sumRep))
createPath [x] cont placeholder =
  case NameSpace.lookup x cont of
    Just __ -> Nothing
    Nothing -> Just (NameSpace.insert (NameSpace.Pub x) placeholder cont)
createPath [] cont _placeholder =
  Just cont
createPath (x : xs) cont placeholder =
  case NameSpace.lookup x cont of
    Just (Record def ty) ->
      continuePath def ty
    Nothing -> continuePath NameSpace.empty Nothing
    Just __ -> Nothing
  where
    continuePath next typ =
      createPath xs next placeholder
        |> fmap (insertRecordCont typ)
    insertRecordCont typ ins =
      NameSpace.insertPublic x (Record ins typ) cont

-------------------------------------------------------------------------------
-- Generalized Helpers
--------------------------------------------------------------------------------

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
  Symbol ->
  Cont (Definition term ty sumRep) ->
  Maybe (t (Definition term ty sumRep))
lookupGen extraLookup key T {currentNameSpace} =
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
      nameSymb = NameSymbol.fromSymbol key
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
