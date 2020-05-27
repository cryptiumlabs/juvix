{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- - Serves as the context for lower level programs of the =Juvix=
--   Programming Language
-- - This is parameterized per phase which may store the type and
--   term in slightly different ways
module Juvix.Core.Common.Context where

import Control.Lens
import qualified Data.HashSet as Set
import qualified Data.Text as Text
import qualified Juvix.Core.Usage as Usage
import Juvix.Library hiding (modify)
import qualified Juvix.Library.HashMap as HashMap

newtype Cont b
  = T (HashMap.T Symbol b)
  deriving (Show, Traversable)
  deriving (Functor) via HashMap.T Symbol
  deriving (Foldable) via HashMap.T Symbol

type T term ty sumRep = Cont (Definition term ty sumRep)

data Definition term ty sumRep
  = Def
      { definitionUsage :: Usage.T,
        definitionTy :: ty,
        definitionTerm :: term
      }
  | Record
      { definitionContents :: T term ty sumRep,
        -- Maybe as I'm not sure what to put here for now
        definitionMTy :: Maybe ty
      }
  | TypeDeclar
      { definitionRepr :: sumRep
      }
  deriving (Show)

makeLensesWith camelCaseFields ''Definition

-- couldn't figure out how to fold lenses
-- once we figure out how to do a fold like
-- foldr (\x y -> x . contents . T  . y) identity brokenKey
-- replace the recursive function with that
lookup :: Symbol -> T term ty sumRep -> Maybe (Definition term ty sumRep)
lookup key (T map) =
  let textKey = textify key
      brokenKey = Text.splitOn "." textKey
      --
      recurse _ Nothing =
        Nothing
      recurse [] x =
        x
      recurse (x : xs) (Just (Record (T contents) _)) =
        recurse xs (HashMap.lookup x contents)
      recurse (_ : _) _ =
        Nothing
   in case brokenKey >>| internText of
        x : xs ->
          recurse xs (HashMap.lookup x map)
        [] ->
          Nothing -- this case never happens

(!?) :: T term ty sumRep -> Symbol -> Maybe (Definition term ty sumRep)
(!?) = flip lookup

modify,
  update ::
    (Definition term ty sumRep -> Maybe (Definition term ty sumRep)) ->
    Symbol ->
    T term ty sumRep ->
    T term ty sumRep
modify f k (T map) = T (HashMap.update f k map)
update = modify

names :: T term ty sumRep -> Set.HashSet Symbol
names (T map) = HashMap.keysSet map

fromList :: [(Symbol, Definition term ty sumRep)] -> T term ty sumRep
fromList = T . HashMap.fromList

toList :: T term ty sumRep -> [(Symbol, Definition term ty sumRep)]
toList (T map) = HashMap.toList map

mapWithKey ::
  (Symbol -> Definition term ty sumRep -> Definition term ty sumRep) ->
  T term ty sumRep ->
  T term ty sumRep
mapWithKey f (T map) = T (HashMap.mapWithKey f map)

open :: Symbol -> T term ty sumRep -> T term ty sumRep
open key (T map) =
  case lookup key (T map) of
    Just (Record (T contents) _) ->
      -- Union takes the first if there is a conflict
      T (HashMap.union contents map)
    Just _ -> T map
    Nothing -> T map
