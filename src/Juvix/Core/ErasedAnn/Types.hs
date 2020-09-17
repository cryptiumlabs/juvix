module Juvix.Core.ErasedAnn.Types where

import Juvix.Core.IR.Types (GlobalName, Universe)
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
  | Con Con
  | Case Symbol (Type primTy primVal) (CaseTree primTy primVal)
  deriving (Show, Eq, Generic)

data Type primTy primVal
  = SymT Symbol
  | Star Universe
  | PrimTy primTy
  | -- TODO: How to deal with dependency?
    Pi Usage.T (Type primTy primVal) (Type primTy primVal)
  | ADT (ADT primTy)
  deriving (Show, Eq, Generic)

data AnnTerm primTy primVal
  = Ann
      { usage :: Usage.T,
        type' :: Type primTy primVal,
        term :: Term primTy primVal
      }
  deriving (Show, Eq, Generic)

data ADT primTy
  = PrimA primTy
  | SumA (ADT primTy) (ADT primTy)
  | ProductA (ADT primTy) (ADT primTy)
  deriving (Show, Eq, Generic)

-- where should we deal with partial constructor application
-- maybe n-ary products & sums are better, second pass to nest them for Michelson, clearer separation
-- include names for LLVM
data Con
  = ConLeft
  | ConRight
  | ConPair
  deriving (Show, Eq, Generic)

data CaseTree primTy primVal
  = Ignore (AnnTerm primTy primVal)
  | BindVar GlobalName (AnnTerm primTy primVal)
  | BindPair GlobalName GlobalName (AnnTerm primTy primVal)
  | SplitLeftRight (GlobalName, AnnTerm primTy primVal) (GlobalName, AnnTerm primTy primVal)
  deriving (Show, Eq, Generic)
