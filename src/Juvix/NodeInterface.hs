{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Juvix.NodeInterface where

import Control.Lens
import Data.Graph.Inductive
import Juvix.Library

-- there is a probably better way of doing this, instead of writing the concrete terms
data ShellNode a
  = ShellAuxs {_prim :: a, _aux1 :: a, _aux2 :: a, _aux3 :: a, _aux4 :: a, _aux5 :: a}
  | -- Need this so aux1 and aux2 need applicative instead of functor
    ShellPrim {_prim :: a}

makeFieldsNoPrefix ''ShellNode

-- Leave these as GADTs for now!
data Primary where
  Primary :: Node → Primary
  Free :: Primary

deriving instance Show Primary

data Auxiliary
  = Auxiliary Node
  | FreeNode
  deriving (Show)

instance Semigroup Auxiliary where
  (<>) FreeNode a = a
  (<>) a FreeNode = a
  -- Poor instance but we are just using it
  (<>) a _ = a

-- Need this for the lens instance ☹
instance Monoid Auxiliary where
  mempty = FreeNode

type Prim a = HasPrim a Primary

type Aux1 a = (Prim a, HasAux1 a Auxiliary)

type Aux2 a = (Aux1 a, HasAux2 a Auxiliary)

type Aux3 a = (Aux2 a, HasAux3 a Auxiliary)

type Aux4 a = (Aux3 a, HasAux4 a Auxiliary)

type Aux5 a = (Aux4 a, HasAux5 a Auxiliary)

auxToPrimary ∷ Auxiliary → Primary

auxToPrimary (Auxiliary node) = Primary node
auxToPrimary FreeNode = Free

auxToNode ∷ Auxiliary → Maybe Node

auxToNode (Auxiliary node) = Just node
auxToNode FreeNode = Nothing
