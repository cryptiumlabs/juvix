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
import qualified Data.HashSet as Set
import qualified Data.Text as Text
import Juvix.Core.Common.Context.Precedence
import qualified Juvix.Core.Common.NameSpace as NameSpace
import qualified Juvix.Core.Common.NameSymbol as NameSymbol
import qualified Juvix.Core.Usage as Usage
import Juvix.Library hiding (modify)
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
  deriving (Show, Generic)

-- not using lenses anymore but leaving this here anyway
makeLensesWith camelCaseFields ''Definition

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

-- couldn't figure out how to fold lenses
-- once we figure out how to do a fold like
-- foldr (\x y -> x . contents . T  . y) identity brokenKey
-- replace the recursive function with that
lookupGen extraLookup key T {currentNameSpace, topLevelMap} =
  let recurse _ Nothing =
        Nothing
      recurse [] x =
        x
      recurse (x : xs) (Just (Record namespace _)) =
        recurse xs (NameSpace.lookup x namespace)
      recurse (_ : _) _ =
        Nothing
   in case NameSymbol.fromSymbol key of
        x :| xs ->
          NameSpace.lookupInternal x currentNameSpace
            |> extraLookup x
            |> \case
              Just x -> traverse (recurse xs . Just) x
              Nothing -> Nothing

lookup ::
  Symbol -> T term ty sumRep -> Maybe (From (Definition term ty sumRep))
lookup key t@(T {topLevelMap}) =
  let f x currentLookup =
        fmap Current currentLookup <|> fmap Outside (HashMap.lookup x topLevelMap)
   in lookupGen f key t

lookupCurrent ::
  Symbol -> T term ty sumRep -> Maybe (NameSpace.From (Definition term ty sumRep))
lookupCurrent =
  lookupGen (\_ currentLookup -> currentLookup)

(!?) ::
  T term ty sumRep -> Symbol -> Maybe (From (Definition term ty sumRep))
(!?) = flip lookup

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

currentNames :: T term ty sumRep -> NameSpace.List (Definition term ty sumRep)
currentNames T {currentNameSpace} = HashMap.keysSet map

fromList :: [(Symbol, Definition term ty sumRep)] -> T term ty sumRep
fromList = T . HashMap.fromList

toList :: T term ty sumRep -> [(Symbol, Definition term ty sumRep)]
toList (T map) = HashMap.toList map

mapWithKey ::
  (Symbol -> Definition term ty sumRep -> Definition term ty sumRep) ->
  T term ty sumRep ->
  T term ty sumRep
mapWithKey f (T map) = T (HashMap.mapWithKey f map)
-- TODO :: change this to an include
-- open :: Symbol -> T term ty sumRep -> T term ty sumRep
-- open key (T map) =
--   case lookup key (T map) of
--     Just (Record (T contents) _) ->
--       -- Union takes the first if there is a conflict
--       T (HashMap.union contents map)
--     Just _ -> T map
--     Nothing -> T map
