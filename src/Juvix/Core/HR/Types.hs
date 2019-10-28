module Juvix.Core.HR.Types where

import           Juvix.Core.Usage
import           Juvix.Library

--checkable terms
data Term primTy primVal
  = Star Natural --sort i
  | PrimTy primTy --primitive type
  | Pi Usage (Term primTy primVal) (Term primTy primVal) --function type
  | Lam Symbol (Term primTy primVal) --abstraction
  | Elim (Elim primTy primVal) --elimination
  deriving (Show, Eq, Generic)

--inferable terms
data Elim primTy primVal
  = Var Symbol --variable
  | Prim primVal --primitive constant
  | App (Elim primTy primVal) (Term primTy primVal) --application
  | Ann Usage (Term primTy primVal) (Term primTy primVal) --type & usage annotation
  deriving (Show, Eq, Generic)
