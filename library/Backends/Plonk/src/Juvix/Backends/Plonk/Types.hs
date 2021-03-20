module Juvix.Backends.Plonk.Types where

import Juvix.Core.ErasedAnn
import Juvix.Library hiding (Type)

data PrimVal f
  = PConst f
  | -- UnOps
    PExp
  | PDup
  | PIsZero
  | PNot
  | PShL
  | PShR
  | PRotL
  | PRotR
  | PAssertEq
  | PAssertIt
  | -- BinOps
    PAdd
  | PSub
  | PMul
  | PDiv
  | PMod
  | PAnd
  | POr
  | PXor
  | -- CompOps
    PGt
  | PGte
  | PLt
  | PLte
  | PEq
  deriving (Show, Eq, Generic, Data)

newtype PrimTy f = PrimTy f

-- FF: Finite field
-- Finite field is the only possible type in Plonk
data FF = FF
  deriving (Show, Eq, Generic, Data)

type FFType = Type FF

type FFTerm f = Term FF (PrimVal f)

type FFAnnTerm f = AnnTerm FF (PrimVal f)
