module Juvix.Backends.Plonk.Types
  ( CompilationError (..),
    AnnTerm,
    Term,
    Type,
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
    isConst,
    isBinOp,
    -- isUnOp
  )
where

import qualified Juvix.Core.Application as App
import qualified Juvix.Core.ErasedAnn as ErasedAnn
import qualified Juvix.Core.IR.Types as IR
import qualified Juvix.Core.Parameterisation as P
import Juvix.Library hiding (Type)

isConst :: PrimVal f -> Bool
isConst (PConst _) = True
isConst _ = False

isBinOp :: PrimVal f -> Bool
isBinOp = \case
  PAdd -> True 
  PSub -> True
  PMul -> True
  PDiv -> True
  PExp -> True
  PMod -> True
  PAnd -> True
  POr -> True
  PXor -> True
  PGt -> True
  PGte -> True
  PLt -> True
  PLte -> True
  PEq -> True
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
  deriving (Show, Eq, Generic, Data)

data PrimTy f
  = PField
  | PInt
  | PBool
  | PApplication (PrimTy f) (NonEmpty (PrimTy f))
  deriving (Show, Eq, Generic)

type Return' ext f = App.Return' ext (P.PrimType (PrimTy f)) (PrimVal f)

type ReturnIR f = Return' IR.NoExt f

type ReturnHR f = Return' ErasedAnn.T f

type Take f = App.Take (P.PrimType (PrimTy f)) (PrimVal f)

type Arg' ext f = App.Arg' ext (P.PrimType (PrimTy f)) (PrimVal f)

type ArgIR f = Arg' IR.NoExt f

type ArgHR f = Arg' ErasedAnn.T f

type PrimVal' ext f = Return' ext f

type PrimValIR f = PrimVal' IR.NoExt f

type PrimValHR f = PrimVal' ErasedAnn.T f

type Type f = ErasedAnn.Type (PrimTy f)

type Term f = ErasedAnn.Term (PrimTy f) (PrimVal f)

type AnnTerm f = ErasedAnn.AnnTerm (PrimTy f) (PrimVal f)

-- type Term = ErasedAnn.AnnTerm PrimTy PrimVal

newtype CompilationError
  = NotYetImplemented Text
  deriving (Show, Eq, Generic)
