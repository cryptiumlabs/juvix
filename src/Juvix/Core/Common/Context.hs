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
import qualified Juvix.Core.Usage as Usage
import Juvix.Library hiding (modify)
import qualified Juvix.Library.HashMap as HashMap

newtype Cont b
  = T (HashMap.T Symbol b)
  deriving (Show, Traversable)
  deriving (Functor) via HashMap.T Symbol
  deriving (Foldable) via HashMap.T Symbol

type T term ty = Cont (Definition term ty)

data Definition term ty
  = Def
      { definitionUsage :: Usage.T,
        definitionTy :: ty,
        definitionTerm :: term
      }
  deriving (Show)

makeLensesWith camelCaseFields ''Definition

lookup :: Symbol -> T term ty -> Maybe (Definition term ty)
lookup key (T map) = HashMap.lookup key map

(!?) :: T term ty -> Symbol -> Maybe (Definition term ty)
(!?) = flip lookup

modify,
  update ::
    (Definition term ty -> Maybe (Definition term ty)) -> Symbol -> T term ty -> T term ty
modify f k (T map) = T (HashMap.update f k map)
update = modify

names :: T term ty -> Set.HashSet Symbol
names (T map) = HashMap.keysSet map

fromList :: [(Symbol, Definition term ty)] -> T term ty
fromList = T . HashMap.fromList

toList :: T term ty -> [(Symbol, Definition term ty)]
toList (T map) = HashMap.toList map

mapWithKey ::
  (Symbol -> Definition term ty -> Definition term ty) -> T term ty -> T term ty
mapWithKey f (T map) = T (HashMap.mapWithKey f map)
