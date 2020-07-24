module Juvix.Backends.Michelson.Datatypes.Types where

import qualified Data.HashMap.Strict as HM
import qualified Juvix.Backends.Michelson.Parameterisation as M
import qualified Juvix.Core.Erasure.Types as E
import qualified Juvix.Core.IR as IR
import Juvix.Library hiding (Datatype, Product, Sum, Type)
import qualified Michelson.Untyped as MU
import qualified Michelson.Untyped.Instr as Instr

type GlobalName = IR.GlobalName

type PreGlobals = HM.HashMap IR.GlobalName ErasedGlobal

data ErasedGlobal
  = EGDatatype GlobalName [EDataCon]
  | EGDataCon EDataCon
  | EGFunction GlobalName Value (NonEmpty EFunClause)
  | EGAbstract

data EDataCon
  = EDataCon
      { eDataConName :: GlobalName,
        eDataConType :: Value
      }

data EFunClause
  = EFunClause [Pattern] Term

type PostGlobals = HM.HashMap IR.GlobalName Global

data Global
  = GDatatype
      { datatypeName :: GlobalName,
        datatypeADT :: ADT
      }
  | GDataCon
      { dataconName :: GlobalName,
        dataconDatatype :: GlobalName,
        dataconIndex :: Int,
        dataconType :: Type
      }
  | GFunction
      { funType :: Type,
        funCases :: Cases
      }

data ADT
  = Prim MU.Type
  | Sum ADT ADT
  | Product ADT ADT

data Con
  = LeftCon
  | RightCon
  | PairCon

data Cases
  = Terminal Term
  | BindVar GlobalName Cases
  | BindPair GlobalName GlobalName Cases
  | LeftRight GlobalName Cases GlobalName Cases

data PatternOrTerm
  = Final [(Pattern, IR.Term M.PrimTy M.PrimVal)]
  | Inter [(Pattern, PatternOrTerm)]

{- Aliases. -}

type Term = E.Term M.PrimTy M.PrimVal

type Type = E.Type M.PrimTy

type Value = IR.Value M.PrimTy M.PrimVal

type Datatype = IR.Datatype M.PrimTy M.PrimVal

type DataCon = IR.DataCon M.PrimTy M.PrimVal

type Pattern = IR.Pattern M.PrimTy M.PrimVal
