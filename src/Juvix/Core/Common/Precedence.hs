module Juvix.Core.Common.Precedence
(default', left, right, application, Precedence, nonAssoc, defaultPred)
  where

import Juvix.Library (Eq, Show, Int, Symbol)

data Associativity = Left
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

defaultPred :: Symbol -> Precedence
defaultPred "+" = left 6
defaultPred "-" = left 6
defaultPred "*" = left 7
defaultPred "/" = left 7
defaultPred "&&" = right 3
defaultPred "||" = right 3
defaultPred "==" = nonAssoc 4
defaultPred "/=" = nonAssoc 4
defaultPred "/=" = nonAssoc 4
defaultPred "<" = nonAssoc 4
defaultPred "<=" = nonAssoc 4
defaultPred ">" = nonAssoc 4
defaultPred ">=" = nonAssoc 4
defaultPred _ = default'
