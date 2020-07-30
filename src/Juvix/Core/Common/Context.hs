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

import Control.Lens
import qualified Data.HashSet as Set
import qualified Data.Text as Text
import Juvix.Core.Common.Context.Precedence
import qualified Juvix.Core.Usage as Usage
import Juvix.Library hiding (modify)
import qualified Juvix.Library.HashMap as HashMap

-- TODO :: Put protected here
data NameSapce b
  = NameSpace
    { public :: HashMap.T Symbol b
    , private :: HashMap.T Symbol b
    }
  deriving (Show)


data Cont b
  = T
    { currentNameSpace :: NameSapce b
    , currentName :: Symbol
    , topLevelMap :: HashMap.T Symbol b
    }
  deriving (Show)

type T term ty sumRep = Cont (Definition term ty sumRep)

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
      { definitionContents :: NameSapce (Definition term ty sumRep),
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

empty :: Symbol -> Cont b
empty sym = T { currentNameSpace = NameSpace HashMap.empty HashMap.empty
          , currentName = sym
          , topLevelMap = HashMap.empty
          }

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

add ::
  Symbol ->
  Definition term ty sumRep ->
  T term ty sumRep ->
  T term ty sumRep
add sy term (T map) = T $ HashMap.insert sy term map

remove ::
  Symbol -> T term ty sumRep -> T term ty sumRep
remove sy (T map) = T $ HashMap.delete sy map

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
