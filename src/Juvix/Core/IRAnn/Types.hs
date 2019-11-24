module Juvix.Core.IRAnn.Types where

import qualified Juvix.Core.IR.Types as IR
import Juvix.Core.Usage
import Juvix.Library

data Term primTy primVal
  = Star Natural
  | PrimTy primTy
  | Pi Usage (Term primTy primVal) (Term primTy primVal)
  | Lam (Term primTy primVal)
  | Elim (AnnElim primTy primVal)
  deriving (Show, Eq, Generic)

data Elim primTy primVal
  = Bound Natural
  | Free IR.Name
  | Prim primVal
  | App (AnnElim primTy primVal) (Term primTy primVal)
  | Ann Usage (Term primTy primVal) (Term primTy primVal)
  deriving (Show, Eq, Generic)

type AnnTerm primTy primVal = (Term primTy primVal, Usage, Term primTy primVal)

type AnnElim primTy primVal = (Elim primTy primVal, Usage, Term primTy primVal)
