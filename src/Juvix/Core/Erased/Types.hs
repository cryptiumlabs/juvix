module Juvix.Core.Erased.Types where

import qualified Juvix.Core.Usage as Usage
import Juvix.Library hiding (Type)
import qualified Juvix.Library.HashMap as Map

data Term primVal
  = Var Symbol
  | Prim primVal
  | -- TODO ∷ add proper lam with capture and arguments here!
    Lam Symbol (Term primVal)
  | Let Symbol (Term primVal) (Term primVal)
  | App (Term primVal) (Term primVal)
  deriving (Show, Eq, Generic)

data Type primTy
  = SymT Symbol
  | Star Natural
  | PrimTy primTy
  | -- TODO: How to deal with dependency?
    Pi Usage.T (Type primTy) (Type primTy)
  deriving (Show, Eq, Generic)

type TypeAssignment primTy = Map.T Symbol (Type primTy)

data EvaluationError primVal
  = PrimitiveApplicationError primVal primVal
  deriving (Show, Eq, Generic)
