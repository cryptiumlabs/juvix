module Juvix.Core.Common.Context.Precedence
  ( default',
    left,
    right,
    application,
    Precedence(..),
    Associativity(..),
    nonAssoc,
    fixity,
  )
where

import Juvix.Library (Eq, Int, Show, Symbol)

data Associativity
  = Left
  | Right
  | NonAssoc
  deriving (Eq, Show)

data Precedence = Pred Associativity Int
  deriving (Eq, Show)

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
