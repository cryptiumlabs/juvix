module Juvix.Core.ErasedAnn.Types where

import Juvix.Core.IR.Types (Universe)
import Juvix.Library hiding (Type)
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.Usage as Usage

data Term primTy primVal
  = Var NameSymbol.T
  | Prim primVal
  | LamM
      { capture :: [NameSymbol.T], -- Capture
        arguments :: [NameSymbol.T], -- Arguments
          -- the Term in AnnTerm is not lam!
        body :: AnnTerm primTy primVal
      }
  | PairM (AnnTerm primTy primVal) (AnnTerm primTy primVal)
  | UnitM
  | AppM (AnnTerm primTy primVal) [AnnTerm primTy primVal]
  deriving (Show, Eq, Generic)

data Type primTy
  = SymT NameSymbol.T
  | Star Universe
  | PrimTy primTy
  | -- TODO: How to deal with dependency?
    Pi Usage.T (Type primTy) (Type primTy)
  | Sig Usage.T (Type primTy) (Type primTy)
  | UnitTy
  deriving (Show, Eq, Generic)

data AnnTerm primTy primVal
  = Ann
      { usage :: Usage.T,
        type' :: Type primTy,
        term :: Term primTy primVal
      }
  deriving (Show, Eq, Generic)
