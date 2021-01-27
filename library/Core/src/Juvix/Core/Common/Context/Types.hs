{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}

module Juvix.Core.Common.Context.Types where

import Control.Lens hiding ((|>))
import Juvix.Core.Common.Context.Precedence
import qualified Juvix.Core.Common.NameSpace as NameSpace
import Juvix.Library
import qualified Juvix.Library.HashMap as HashMap
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.Usage as Usage

data Cont b
  = T
      { currentNameSpace :: NameSpace.T b,
        currentName :: NameSymbol.T,
        topLevelMap :: HashMap.T Symbol b
      }
  deriving (Show, Eq, Generic, Data, Functor, Foldable)

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
  | Information
      { definitionInfo :: [Information]
      }
  | -- Signifies that this path is the current module, and that
    -- we should search the currentNameSpace from here
    CurrentNameSpace
  deriving (Show, Generic, Eq, Data)

data Information
  = Prec Precedence
  deriving (Show, Generic, Eq, Data)

-- not using lenses anymore but leaving this here anyway
makeLensesWith camelCaseFields ''Definition

data PathError
  = VariableShared NameSymbol.T
  deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Special Names
--------------------------------------------------------------------------------

topLevelName :: IsString p => p
topLevelName = "TopLevel"
