module Juvix.Core.HR.Types where

import Juvix.Core.Usage
import Juvix.Library

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

data Term2 primTy primVal
  = Star2 Natural
  | Prim2 (primTy (Term2 primTy primVal))
  | Lam2 Symbol (Term2 primTy primVal)
  deriving (Generic)

data PrimTyEx term
  = PrimTy0
  | PrimPairT term term
  deriving (Generic)

type Term2Ex = Term2 PrimTyEx ()

test1 ∷ Term2Ex
test1 = Star2 0

test2 ∷ Term2Ex
test2 = Prim2 (PrimTy0)

test3 ∷ Term2Ex
test3 = Prim2 (PrimPairT (Star2 0) (Star2 1))
