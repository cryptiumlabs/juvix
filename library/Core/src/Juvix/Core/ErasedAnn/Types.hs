module Juvix.Core.ErasedAnn.Types where

import Juvix.Core.IR.Types (Universe)
import Juvix.Library hiding (Type)
import qualified Juvix.Library.Usage as Usage

data Term primTy primVal
  = Var Symbol
  | Prim primVal
  | LamM
      { capture :: [Symbol], -- Capture
        arguments :: [Symbol], -- Arguments
          -- the Term in AnnTerm is not lam!
        body :: AnnTerm primTy primVal
      }
  | PairM (AnnTerm primTy primVal) (AnnTerm primTy primVal)
  | AppM (AnnTerm primTy primVal) [AnnTerm primTy primVal]
  deriving (Show, Eq, Generic)

data Type primTy
  = SymT Symbol
  | Star Universe
  | PrimTy primTy
  | -- TODO: How to deal with dependency?
    Pi Usage.T (Type primTy) (Type primTy)
  | Sig Usage.T (Type primTy) (Type primTy)
  deriving (Show, Eq, Generic)

data AnnTerm primTy primVal
  = Ann
      { usage :: Usage.T,
        type' :: Type primTy,
        term :: Term primTy primVal
      }
  deriving (Show, Eq, Generic)
