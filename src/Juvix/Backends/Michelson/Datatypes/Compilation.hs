module Juvix.Backends.Michelson.Datatypes.Compilation where

import qualified Data.HashMap.Strict as HM
import Juvix.Backends.Michelson.Datatypes.Types
import qualified Juvix.Backends.Michelson.Parameterisation as M
import qualified Juvix.Core.Erasure.Types as E
import qualified Juvix.Core.IR as IR
import Juvix.Library hiding (Datatype, Product, Sum, Type)
import qualified Michelson.Untyped as MU
import qualified Michelson.Untyped.Instr as Instr

{-
 - 1. Transform globals.
 - 2. Transform pattern matches.
 - 3. Substitute.
 -}

transformGlobals :: PreGlobals -> PostGlobals
transformGlobals globals =
  flip HM.mapWithKey globals $ \key val ->
    case val of
      IR.GDatatype d -> transformDatatype globals d
      IR.GDataCon c -> transformDataCon globals c
      IR.GFunction f -> undefined
      IR.GAbstract _ _ -> undefined

transformDatatype :: PreGlobals -> Datatype -> Global
transformDatatype globals (IR.Datatype name _ _ cons) =
  let valueToADT v =
        case v of
          IR.VPrimTy (M.PrimTy t) -> Prim t
          IR.VNeutral (IR.NFree (IR.Global g)) ->
            let Just v = HM.lookup g globals
             in case v of
                  -- Note that this will break on mutual recursive definitions.
                  IR.GDatatype d -> let GDatatype _ t = transformDatatype globals d in t
          IR.VPi _ arg res ->
            Product (valueToADT arg) (valueToADT res)
      dataconToADT (IR.DataCon _ ty) =
        case ty of
          -- TODO: Is this what we should expect? We just want the args of the constructor.
          IR.VPi _ arg res -> valueToADT arg
      dataconsToADT cons =
        case cons of
          x : [] -> dataconToADT x
          x : xs ->
            let x' = dataconToADT x
                xs' = dataconsToADT xs
             in Sum x' xs'
   in GDatatype name (dataconsToADT cons)

transformDataCon :: PreGlobals -> DataCon -> Global
transformDataCon globals (IR.DataCon name ty) =
  let datatypeName = dataConTypeName ty
      Just (IR.GDatatype (IR.Datatype _ _ _ cons)) = HM.lookup datatypeName globals
      [(_, index)] = if length cons == 1 then [(undefined, -1)] else filter ((==) name . IR.conName . fst) $ zip cons [0 ..]
      valueToTy v =
        case v of
          IR.VPrimTy (M.PrimTy t) -> E.PrimTy (M.PrimTy t)
          IR.VNeutral (IR.NFree (IR.Global g)) ->
            let Just v = HM.lookup g globals
             in case v of
                  IR.GDatatype d ->
                    let GDatatype _ adt = transformDatatype globals d
                     in E.PrimTy (M.PrimTy (adtToMichelsonType adt))
          IR.VPi usage arg res ->
            E.Pi usage (valueToTy arg) (valueToTy res)
   in GDataCon name datatypeName index (valueToTy ty)

dataConTypeName :: Value -> IR.GlobalName
dataConTypeName (IR.VNeutral (IR.NFree (IR.Global n))) = n
dataConTypeName (IR.VPi _ _ r) = dataConTypeName r

adtToMichelsonType :: ADT -> MU.Type
adtToMichelsonType adt =
  case adt of
    Prim p -> p
    Sum x y -> MU.Type (MU.TOr "" "" (adtToMichelsonType x) (adtToMichelsonType y)) ""
    Product x y -> MU.Type (MU.TPair "" "" (adtToMichelsonType x) (adtToMichelsonType y)) ""

datatypesToMichelson :: PreGlobals -> Term -> Term
datatypesToMichelson globals term = rec term
  where
    postGlobals = transformGlobals globals
    rec term =
      case term of
        E.Var sym vty ->
          case HM.lookup sym postGlobals of
            Just (GDataCon name datatypeName index ty) ->
              let Just (GDatatype _ adt) = HM.lookup datatypeName postGlobals
               in dataconToMichelson ty adt index
            _ -> term
        E.Prim _ _ -> term
        E.Lam sym body ty -> E.Lam sym (rec body) (typeToMichelson postGlobals ty)
        E.Let sym bind body (tyb, tyr) -> E.Let sym (rec bind) (rec body) (typeToMichelson postGlobals tyb, typeToMichelson postGlobals tyr)
        E.App f x ty -> E.App (rec f) (rec x) (typeToMichelson postGlobals ty)
        _ -> term

typeToMichelson :: PostGlobals -> E.Type M.PrimTy -> E.Type M.PrimTy
typeToMichelson globals ty = rec ty
  where
    rec ty =
      case ty of
        E.SymT sym ->
          case HM.lookup sym globals of
            Just (GDatatype _ adt) -> E.PrimTy (M.PrimTy (adtToMichelsonType adt))
            Nothing -> ty
        E.Pi u a r -> E.Pi u (typeToMichelson globals a) (typeToMichelson globals r)
        _ -> ty

indexToLeftRight :: Int -> MU.Type -> [MU.InstrAbstract M.Op]
indexToLeftRight (-1) _ = []
indexToLeftRight 0 t = [Instr.LEFT "" "" "" "" t]
indexToLeftRight 1 t = [Instr.RIGHT "" "" "" "" t]
indexToLeftRight n t = indexToLeftRight (n - 1) t <> [Instr.RIGHT "" "" "" "" t]

-- TODO:
-- Should find n args, then take n args as variables, then apply pair(s), then left/right as appropriate.
opSequenceToApply :: Type -> [MU.InstrAbstract M.Op] -> Term
opSequenceToApply ty [] = E.Lam "0" (E.Var "0" ty) ty
opSequenceToApply ty@(E.Pi _ arg res) (op : []) = E.Lam "0" (E.App (E.Prim (M.Inst op) ty) (E.Var "0" arg) res) ty
opSequenceToApply ty@(E.Pi _ arg1 ty1@(E.Pi _ arg2 res)) (op1 : op2 : []) = E.Lam "0" (E.App (E.App (E.Prim (M.Inst op1) undefined) (E.Var "0" arg1) undefined) (E.Var "1" arg2) res) ty

dataconToMichelson :: Type -> ADT -> Int -> Term
dataconToMichelson ty adt index =
  let pty = adtToMichelsonType adt
      insts = indexToLeftRight index pty
   in opSequenceToApply ty (tuplify (nargs ty) <> insts)

tuplify :: Int -> [MU.InstrAbstract M.Op]
tuplify 0 = []
tuplify 1 = []
tuplify n = (Instr.PAIR "" "" "" "") : tuplify (n - 1)

nargs :: Type -> Int
nargs (E.Pi _ _ res) = 1 + nargs res
nargs _ = 0
{-
 -
patternToMichelson :: PatternAndTerm -> Term
patternToMichelson (patt, term) =
  case patt of
    E.PCon _ patts ->
      -- turn into de-construction followed by application
      undefined
    E.PVar v ->
      -- turn into application of function (\v -> term)
      undefined
    E.PDot _ ->
      -- what is PDot
      undefined
    E.PPrim _ ->
      -- not supporting this for now, later turn into EQ but we need a default case
      undefined

patternsToCases :: [PatternAndTerm] -> Cases
patternsToCases [only] = OneCase only
patternsToCases [first, second] = TwoCase first second
patternsToCases (x : xs) = NestCase x (patternsToCases xs)

-}
