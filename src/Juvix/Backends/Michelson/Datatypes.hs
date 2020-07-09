module Juvix.Backends.Michelson.Datatypes where

import qualified Data.HashMap.Strict as HM
import qualified Juvix.Backends.Michelson.Parameterisation as M
import qualified Juvix.Core.Erasure.Types as E
import qualified Juvix.Core.IR as IR
import Juvix.Library hiding (Datatype, Product, Sum, Type)
import qualified Michelson.Untyped as MU
import qualified Michelson.Untyped.Instr as Instr

type Globals = IR.Globals M.PrimTy M.PrimVal

type Term = E.Term M.PrimTy M.PrimVal

type Value = IR.Value M.PrimTy M.PrimVal

type Datatype = IR.Datatype M.PrimTy M.PrimVal

type DataCon = IR.DataCon M.PrimTy M.PrimVal

type Pattern = IR.Pattern M.PrimTy M.PrimVal

data ADT
  = Prim MU.Type
  | Sum ADT ADT
  | Product ADT ADT

type PatternAndTerm = (Pattern, Term)

data Cases
  = OneCase PatternAndTerm
  | TwoCase PatternAndTerm PatternAndTerm
  | NestCase PatternAndTerm Cases

data Match
  = Match
      { scrutinee :: Term,
        cases :: Cases
      }

-- need to alter term to add match, cases, adts
-- need to re-order patterns to match one argument at once
-- is this going to be cleaner or less clean that doing it in the michelson backend itself?
-- at least we could separate out the adt/case/match transform though

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
datatypesToMichelson globals term = rec term
  where
    rec term =
      case term of
        E.Var sym vty ->
          case HM.lookup sym globals of
            Just (IR.GDataCon con) ->
              let tyName = dataConTypeName con
                  Just (IR.GDatatype ty) = HM.lookup tyName globals
               in E.Prim (dataconToMichelson ty (IR.conName con)) vty
            _ -> term
        E.Prim _ _ -> term
        E.Lam sym body ty -> E.Lam sym (rec body) ty
        E.Let sym bind body ty -> E.Let sym (rec bind) (rec body) ty
        E.App f x ty -> E.App (rec f) (rec x) ty
        _ -> term

dataConTypeName :: DataCon -> IR.GlobalName
dataConTypeName (IR.DataCon _ ty) = ret ty
  where
    ret (IR.VNeutral (IR.NFree (IR.Global n))) = n
    ret (IR.VPi _ _ r) = ret r

datatypeToMichelsonType :: Datatype -> M.PrimTy
datatypeToMichelsonType = M.PrimTy . adtToMichelsonType . datatypeToADT

indexToLeftRight :: Int -> MU.Type -> MU.ExpandedOp
indexToLeftRight 0 _ = Instr.SeqEx []
indexToLeftRight 1 t = Instr.SeqEx [Instr.PrimEx (Instr.LEFT "" "" "" "" t)]
indexToLeftRight 2 t = Instr.SeqEx [Instr.PrimEx (Instr.RIGHT "" "" "" "" t)]
indexToLeftRight n t = Instr.SeqEx [indexToLeftRight (n - 1) t, Instr.PrimEx (Instr.RIGHT "" "" "" "" t)]

-- will need context of what the encapsulating datatype is to choose the right sum part
dataconToMichelson :: IR.Datatype M.PrimTy M.PrimVal -> IR.GlobalName -> M.PrimVal
dataconToMichelson ty@(IR.Datatype _ _ _ cons) name =
  let con :: DataCon
      Just (con, index) = head $ filter ((==) name . IR.conName . fst) $ zip cons [0 ..]
      adt = dataconsToADT cons
      -- ty = adtToMichelsonType adt
      ty = MU.Type MU.TInt ""
      Instr.SeqEx [Instr.PrimEx lr] = indexToLeftRight index ty
   in -- TODO: Need to fix this type.
      M.Inst lr
