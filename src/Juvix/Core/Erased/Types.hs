module Juvix.Core.Erased.Types where

import Juvix.Library hiding (Type)
import Juvix.Utility

data Term primVal
  = Var Symbol
  | Prim primVal
  | Lam Symbol (Term primVal)
  | App (Term primVal) (Term primVal)
  deriving (Show, Eq, Generic)

data Type primTy
  = Star Natural
  | PrimTy primTy
  | -- TODO: How to deal with dependency?
    Pi (Type primTy) (Type primTy)
  deriving (Show, Eq, Generic)

type TypeAssignment primTy = HashMap Symbol (Type primTy)
