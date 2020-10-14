module Juvix.Core.ErasedAnn.Types where

import Juvix.Core.IR.Types (Universe)
import qualified Juvix.Core.Usage as Usage
import Juvix.Library hiding (Type)

data Term primTy primVal
  = Var Symbol
  | Prim primVal
  | LamM
      { capture :: [Symbol], -- Capture
        arguments :: [Symbol], -- Arguments
          -- the Term in AnnTerm is not lam!
        body :: AnnTerm primTy primVal
      }
  | AppM (AnnTerm primTy primVal) [AnnTerm primTy primVal]
  deriving (Show, Eq, Generic)

data Type primTy primVal
  = SymT Symbol
  | Star Universe
  | PrimTy primTy
  | -- TODO: How to deal with dependency?
    Pi Usage.T (Type primTy primVal) (Type primTy primVal)
  deriving (Show, Eq, Generic)

data AnnTerm primTy primVal
  = Ann
      { usage :: Usage.T,
        type' :: Type primTy primVal,
        term :: Term primTy primVal
      }
  deriving (Show, Eq, Generic)
