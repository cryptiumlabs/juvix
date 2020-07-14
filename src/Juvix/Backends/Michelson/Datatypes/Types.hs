module Juvix.Backends.Michelson.Datatypes.Types where

import qualified Data.HashMap.Strict as HM
import qualified Juvix.Backends.Michelson.Parameterisation as M
import qualified Juvix.Core.Erasure.Types as E
import qualified Juvix.Core.IR as IR
import Juvix.Library hiding (Datatype, Product, Sum, Type)
import qualified Michelson.Untyped as MU
import qualified Michelson.Untyped.Instr as Instr

type GlobalName = IR.GlobalName

type PreGlobals = IR.Globals M.PrimTy M.PrimVal

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
        funCase :: Cases
      }
  | GAbstract
      { -- Can we / do we need to track the usage here?
        abstractTerm :: Term
      }

data ADT
  = Prim MU.Type
  | Sum ADT ADT
  | Product ADT ADT

data Bind
  = ConBind GlobalName [GlobalName]
  | VarBind GlobalName
  | NoBind

type BindAndTerm = (Bind, Term)

data Cases
  = OneCase BindAndTerm
  | TwoCase BindAndTerm BindAndTerm
  | NestCase BindAndTerm Cases

{- Aliases. -}

type Term = E.Term M.PrimTy M.PrimVal

type Type = E.Type M.PrimTy

type Value = IR.Value M.PrimTy M.PrimVal

type Datatype = IR.Datatype M.PrimTy M.PrimVal

type DataCon = IR.DataCon M.PrimTy M.PrimVal

type Pattern = IR.Pattern M.PrimTy M.PrimVal
