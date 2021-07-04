{-# LANGUAGE DeriveGeneric #-}

module Juvix.Context.Precedence
  ( default',
    left,
    right,
    application,
    Precedence (..),
    Associativity (..),
    nonAssoc,
    fixity,
  )
where

import Data.Data
import Data.Hashable (Hashable (..), hash)
import Juvix.Library (Eq, Int, Read, Show, Symbol)

data Associativity
  = Left
  | Right
  | NonAssoc
  deriving (Eq, Show, Data, Generic)

data Precedence = Pred Associativity Int
  deriving (Eq, Show, Data, Generic)

instance Hashable Associativity where
  hash Left = 1
  hash Right = 2
  hash NonAssoc = 3

instance Hashable Precedence where
  hash (Pred assoc num) = hash (hash assoc, hash num)
  deriving (Eq, Show, Read, Data)

default' :: Precedence
default' = Pred Left 9

left :: Int -> Precedence
left = Pred Left

right :: Int -> Precedence
right = Pred Right

nonAssoc :: Int -> Precedence
nonAssoc = Pred NonAssoc

application :: Precedence
application = Pred Right 10

-- From Haskell98 report
-- https://www.haskell.org/onlinereport/decls.html#fixity
fixity :: Symbol -> Precedence
fixity "+" = left 6
fixity "-" = left 6
fixity "*" = left 7
fixity "/" = left 7
fixity "&&" = right 3
fixity "||" = right 3
fixity "==" = nonAssoc 4
fixity "/=" = nonAssoc 4
fixity "<" = nonAssoc 4
fixity "<=" = nonAssoc 4
fixity ">" = nonAssoc 4
fixity ">=" = nonAssoc 4
fixity _ = default'
