{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}

module Juvix.Context.Types where

import Control.Lens hiding ((|>))
import GHC.Show
import qualified Juvix.Context.NameSpace as NameSpace
import qualified Juvix.Context.Open as Open
import Juvix.Context.Precedence
import Juvix.Library
import qualified Juvix.Library.HashMap as HashMap
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.Usage as Usage
import qualified StmContainers.Map as STM
import System.IO.Unsafe (unsafePerformIO)
import Text.Read (Read (readsPrec))

data T term ty sumRep = T
  { currentNameSpace :: Record term ty sumRep,
    currentName :: NameSymbol.T,
    topLevelMap :: HashMap.T Symbol (Definition term ty sumRep),
    reverseLookup :: ReverseLookup
  }
  deriving (Show, Read, Eq, Generic)

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
  = Def (Def term ty)
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
  | SumCon (SumT term ty)
  deriving (Show, Read, Generic, Eq)

data Def term ty = D
  { defUsage :: Maybe Usage.T,
    defMTy :: Maybe ty,
    defTerm :: term,
    defPrecedence :: Precedence
  }
  deriving (Show, Read, Generic, Eq, Data)

data SumT term ty = Sum
  { sumTDef :: Maybe (Def term ty),
    sumTName :: Symbol
  }
  deriving (Show, Read, Generic, Eq, Data)

data Record term ty sumRep = Rec
  { recordContents :: NameSpace.T (Definition term ty sumRep),
    -- Maybe as I'm not sure what to put here for now
    -- TODO ∷ reconsider the type when we have proper module typing up.
    recordMTy :: Maybe ty,
    recordOpenList :: [Open.TName NameSymbol.T],
    recordQualifiedMap :: SymbolMap
  }
  deriving (Show, Read, Generic, Eq)

newtype Information
  = Prec Precedence
  deriving (Show, Read, Generic, Eq, Data)

newtype PathError
  = VariableShared NameSymbol.T
  deriving (Show, Read, Eq)

data WhoUses = Who
  { impExplict :: Open.T,
    modName :: NameSymbol.T,
    symbolMap :: SymbolMap
  }
  deriving (Show, Read, Eq, Generic)

type SymbolMap = STM.Map Symbol SymbolInfo

type ReverseLookup = HashMap.T NameSymbol.T [WhoUses]

-- Note ∷ we don't store the implicit explicit open nature of the symbol
-- this can be found by querying the reverse map and seeing there
-- this is sadly O(n) right now... but can be made faster in the future
data SymbolInfo = SymInfo
  { -- | used notes if the symbol is used and if so in what
    used :: UsedIn,
    -- | mod is the module where the symbol is coming from
    mod :: NameSymbol.T
  }
  deriving (Show, Read, Eq, Generic)

data UsedIn = Func [Symbol] | NotUsed | Yes deriving (Show, Read, Eq, Generic)

instance Show (STM.Map a b) where
  show _ = "map"

instance Read (STM.Map a b) where
  -- I'm sorry :(
  readsPrec _ _ = [(unsafePerformIO $ atomically STM.new, "")]

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

makeLensesWith camelCaseFields ''Def

makeLensesWith camelCaseFields ''Record

-- to avoid refactor we just add _ infront
makeLensesFor
  [ ("currentNameSpace", "_currentNameSpace"),
    ("currentName", "_currentName"),
    ("topLevelMap", "_topLevelMap"),
    ("reverseLookup", "_reverseLookup")
  ]
  ''T

--------------------------------------------------------------------------------
-- Special Names
--------------------------------------------------------------------------------

topLevelName :: IsString p => p
topLevelName = "TopLevel"
