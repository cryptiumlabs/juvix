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
import qualified Juvix.Core.Erased.Ann as ErasedAnn
import qualified Juvix.Core.IR.Types as IR
import qualified Juvix.Core.Parameterisation as P
import Juvix.Library hiding (Type)

data PrimVal f
  = PConst f
  | -- UnOps
    PIsZero
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
  deriving (Show, Read, Eq, Generic, Data)

data PrimTy f
  = PField
  | PInt
  | PBool
  | PApplication (PrimTy f) (NonEmpty (PrimTy f))
  deriving (Show, Read, Eq, Generic)

type Return' ext f = App.Return' ext (P.PrimType (PrimTy f)) (PrimVal f)

type ReturnIR f = Return' IR.T f

type ReturnHR f = Return' ErasedAnn.T f

type Take f = App.Take (P.PrimType (PrimTy f)) (PrimVal f)

type Arg' ext f = App.Arg' ext (P.PrimType (PrimTy f)) (PrimVal f)

type ArgIR f = Arg' IR.T f

type ArgHR f = Arg' ErasedAnn.T f

type PrimVal' ext f = Return' ext f

type PrimValIR f = PrimVal' IR.T f

type PrimValHR f = PrimVal' ErasedAnn.T f

type FFType f = ErasedAnn.Type (PrimTy f)

type FFTerm f = ErasedAnn.Term (PrimTy f) (PrimVal f)

type FFAnnTerm f = ErasedAnn.AnnTerm (PrimTy f) (PrimVal f)

newtype CompilationError f
  = NotYetImplemented Text
  deriving (Show, Eq, Generic)
