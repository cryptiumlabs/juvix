module Juvix.Backends.Plonk.Types
  ( CompilationError (..),
    FFAnnTerm,
    FFTerm,
    FFType,
    PrimValHR,
    PrimValIR,
    PrimVal',
    ArgHR,
    ArgIR,
    Arg',
    Take,
    ReturnHR,
    ReturnIR,
    Return',
    PrimTy (..),
    PrimVal (..),
  )
where

import qualified Juvix.Core.Application as App
import Juvix.Core.ErasedAnn
import qualified Juvix.Core.ErasedAnn.Types as CoreErased
import qualified Juvix.Core.IR.Types as IR
import qualified Juvix.Core.Parameterisation as P
import Juvix.Library hiding (Type)

data PrimVal f
  = PConst f
  | -- UnOps
    PDup
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
  | PExp
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

data PrimTy f
  = PField
  | PInt
  | PBool
  | PApplication (PrimTy f) (NonEmpty (PrimTy f))
  deriving (Show, Eq, Generic, Data)

type Return' ext f = App.Return' ext (P.PrimType (PrimTy f)) (PrimVal f)

type ReturnIR f = Return' IR.NoExt f

type ReturnHR f = Return' CoreErased.T f

type Take f = App.Take (P.PrimType (PrimTy f)) (PrimVal f)

type Arg' ext f = App.Arg' ext (P.PrimType (PrimTy f)) (PrimVal f)

type ArgIR f = Arg' IR.NoExt f

type ArgHR f = Arg' CoreErased.T f

type PrimVal' ext f = Return' ext f

type PrimValIR f = PrimVal' IR.NoExt f

type PrimValHR f = PrimVal' CoreErased.T f

-- FF: Finite field
-- Finite field is the only possible type in Plonk
-- data FF = FF
--   deriving (Show, Eq, Generic, Data)

type FFType f = Type (PrimTy f)

type FFTerm f = Term (PrimTy f) (PrimVal f)

type FFAnnTerm f = AnnTerm (PrimTy f) (PrimVal f)

data CompilationError
  = NotYetImplemented Text
  deriving (Show, Eq, Generic)
