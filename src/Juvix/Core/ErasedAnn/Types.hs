module Juvix.Core.ErasedAnn.Types where

import Juvix.Core.Usage
import Juvix.Library hiding (Type)

data Term primTy primVal
  = Var Symbol
  | Prim primVal
  | Lam Symbol (AnnTerm primTy primVal)
  | LamM
      { capture ∷ [Symbol], -- Capture
        arguments ∷ [Symbol], -- Arguments
          -- the Term in AnnTerm is not lam!
        body ∷ AnnTerm primTy primVal
      }
  | App (AnnTerm primTy primVal) (AnnTerm primTy primVal)
  deriving (Show, Eq, Generic)

data Type primTy primVal
  = SymT Symbol
  | Star Natural
  | PrimTy primTy
  | -- TODO: How to deal with dependency?
    Pi Usage (Type primTy primVal) (Type primTy primVal)
  deriving (Show, Eq, Generic)

type AnnTerm primTy primVal = (Term primTy primVal, Usage, Type primTy primVal)
