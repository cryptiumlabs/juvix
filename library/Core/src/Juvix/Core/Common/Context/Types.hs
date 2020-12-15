{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}

module Juvix.Core.Common.Context.Types where

import Control.Lens hiding ((|>))
import GHC.Show
import Juvix.Core.Common.Context.Precedence
import qualified Juvix.Core.Common.NameSpace as NameSpace
import qualified Juvix.Core.Common.Open as Open
import Juvix.Library
import qualified Juvix.Library.HashMap as HashMap
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.Usage as Usage
import qualified StmContainers.Map as STM

data Cont b
  = T
      { currentNameSpace :: NameSpace.T b,
        currentName :: NameSymbol.T,
        topLevelMap :: HashMap.T Symbol b,
        reverseLookup :: SymbolMap
      }
  deriving (Show, Eq, Generic)

type T term ty sumRep = Cont (Definition term ty sumRep)

type NameSpace term ty sumRep = NameSpace.T (Definition term ty sumRep)

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
  | Record (Record term ty sumRep)
  | TypeDeclar
      { definitionRepr :: sumRep
      }
  | Unknown
      { definitionMTy :: Maybe ty
      }
  | Information
      { definitionInfo :: [Information]
      }
  | -- Signifies that this path is the current module, and that
    -- we should search the currentNameSpace from here
    CurrentNameSpace
  deriving (Show, Generic, Eq)

data Record term ty sumRep
  = Rec
      { recordContents :: NameSpace.T (Definition term ty sumRep),
        -- Maybe as I'm not sure what to put here for now
        recordMTy :: Maybe ty,
        recordOpenList :: [Open.TName NameSymbol.T],
        recordQualifiedMap :: SymbolMap
      }
  deriving (Show, Generic, Eq)

data Information
  = Prec Precedence
  deriving (Show, Generic, Eq, Data)

data PathError
  = VariableShared NameSymbol.T
  deriving (Show, Eq)

data WhoUses
  = Who
      { impExplict :: Open.T,
        modName :: NameSymbol.T,
        symbolMap :: SymbolMap
      }
  deriving (Show, Eq, Generic)

type SymbolMap = STM.Map Symbol SymbolInfo

type ReverseLookup = HashMap.T NameSymbol.T [WhoUses]

data SymbolInfo
  = SymInfo
      { -- | used notes if the symbol is used and if so in what
        used :: UsedIn,
        -- | mod is the module where the symbol is coming from
        mod :: NameSymbol.T
      }
  deriving (Show, Eq, Generic)

data UsedIn = Func [Symbol] | NotUsed | Yes deriving (Show, Eq, Generic)

instance Show (STM.Map a b) where
  show _ = "map"

instance Show (STM (STM.Map a b)) where
  show _ = "STM map"

-- for the sake of our types, we are just going to ignore any value in
-- the STM map
instance Eq (STM a) where
  _ == _ = True

instance Eq (STM.Map a b) where
  _ == _ = True

-- not using lenses anymore but leaving this here anyway
makeLensesWith camelCaseFields ''Definition

makeLensesWith camelCaseFields ''Record
