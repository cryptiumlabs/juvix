module Juvix.Core.Erased.Types where

import Juvix.Core.Usage
import Juvix.Library hiding (Type)
import qualified Juvix.Library.HashMap as Map

data Term primVal
  = Var Symbol
  | Prim primVal
  | Lam Symbol (Term primVal)
  | App (Term primVal) (Term primVal)
  deriving (Show, Eq, Generic)

data Type primTy
  = SymT Symbol
  | Star Natural
  | PrimTy primTy
  | -- TODO: How to deal with dependency?
    Pi Usage (Type primTy) (Type primTy)
  deriving (Show, Eq, Generic)

type TypeAssignment primTy = Map.Map Symbol (Type primTy)
