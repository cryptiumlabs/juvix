module Juvix.Core.Erased.Datatypes where

import qualified Juvix.Backends.Michelson.Parameterisation as M
import qualified Juvix.Core.Erased.Types as E
import qualified Juvix.Core.IR as IR
import Juvix.Library hiding (Datatype, Product, Sum, Type)
import qualified Michelson.Untyped as MU

type Globals = IR.Globals M.PrimTy M.PrimVal

type Term = E.Term M.PrimTy M.PrimVal

type Value = E.Value M.PrimTy M.PrimVal

type Datatype = E.Datatype M.PrimTy M.PrimVal

type DataCon = E.DataCon M.PrimTy M.PrimVal

type Pattern = E.Pattern M.PrimTy M.PrimVal

data ADT
  = Prim MU.Type
  | Sum ADT ADT
  | Product ADT ADT

data Cases
  = OneCase Pattern
  | TwoCase Pattern Pattern

data Match
  = Match
      { scrutinee :: Term,
        cases :: Cases
      }

datatypeToADT :: Datatype -> ADT
datatypeToADT (E.Datatype _ _ _ cons) = dataconsToADT cons

dataconsToADT :: [DataCon] -> ADT
dataconsToADT cons =
  case cons of
    x : [] -> dataconToADT x
    x : xs ->
      let x' = dataconToADT x
          xs' = dataconsToADT xs
       in Sum x' xs'

dataconToADT :: DataCon -> ADT
dataconToADT (E.DataCon _ ty) =
  case ty of
    -- TODO: Is this what we should expect? We just want the args of the constructor.
    E.VPi _ arg res -> valueToADT arg

valueToADT :: Value -> ADT
valueToADT v =
  case v of
    E.VPrimTy (M.PrimTy t) -> Prim t
    E.VPi _ arg res ->
      Product (valueToADT arg) (valueToADT res)

adtToMichelsonType :: ADT -> MU.Type
adtToMichelsonType adt =
  case adt of
    Prim p -> p
    Sum x y -> MU.Type (MU.TOr "" "" (adtToMichelsonType x) (adtToMichelsonType y)) ""
    Product x y -> MU.Type (MU.TPair "" "" (adtToMichelsonType x) (adtToMichelsonType y)) ""

datatypesToMichelson :: Globals -> Term -> Term
datatypesToMichelson globals term = undefined

datatypeToMichelsonType :: Datatype -> M.PrimTy
datatypeToMichelsonType = M.PrimTy . adtToMichelsonType . datatypeToADT

-- will need context of what the encapsulating datatype is to choose the right sum part
dataconToMichelson :: DataCon -> M.PrimVal
dataconToMichelson = undefined
