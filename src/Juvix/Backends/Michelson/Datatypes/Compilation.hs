module Juvix.Backends.Michelson.Datatypes.Compilation where

import qualified Data.HashMap.Strict as HM
import qualified Juvix.Backends.Michelson.Compilation.Types as M
import Juvix.Backends.Michelson.Datatypes.Types
import qualified Juvix.Backends.Michelson.Parameterisation as M
import qualified Juvix.Core.Erasure.Types as E
import qualified Juvix.Core.IR as IR
import qualified Juvix.Core.Usage as Usage
import Juvix.Library hiding (Datatype, Product, Sum, Type)
import qualified Michelson.Untyped as MU
import qualified Michelson.Untyped.Instr as Instr

transformGlobals :: PreGlobals -> PostGlobals
transformGlobals globals =
  flip HM.mapMaybeWithKey globals $ \key val ->
    case val of
      EGDatatype n cs -> pure (transformDatatype globals n cs)
      EGDataCon c -> pure (transformDataCon globals c)
      EGFunction n t c -> pure (transformFunction globals n t c)

transformFunction :: PreGlobals -> GlobalName -> Value -> NonEmpty EFunClause -> Global
transformFunction globals name ty clauses =
  -- Calculate ADT for argument.
  let IR.VPi _ (IR.VNeutral (IR.NFree (IR.Global g))) _ = ty
      Just (EGDatatype n cs) = HM.lookup g globals
      GDatatype _ adt = transformDatatype globals n cs
      cases = clausesToCases clauses
   in GFunction undefined cases

clausesToCases :: NonEmpty EFunClause -> Cases
clausesToCases clauses = case clauses of
  -- Single variable.
  (EFunClause [IR.PVar v] term) :| [] ->
    BindVar (varToSym v) (Terminal term)
  -- Product type.
  (EFunClause [IR.PCon n [IR.PVar a, IR.PVar b]] term) :| [] ->
    BindPair (varToSym a) (varToSym b) (Terminal term)
  -- Sum type.
  (EFunClause [IR.PCon a pattA] termA) :| [EFunClause [IR.PCon b pattB] termB] ->
    let bindA = "bind_a"
        bindB = "bind_b"
        caseA = clausesToCases ((EFunClause pattA termA) :| [])
        caseB = clausesToCases ((EFunClause pattB termB) :| [])
     in -- TODO figure out ordering
        -- TODO carry bind names
        LeftRight bindA caseA bindB caseB

varToSym :: IR.PatternVar -> Symbol
varToSym = intern . show

casesToTerm :: Cases -> Term
casesToTerm cases = case cases of
  Terminal t -> t
  BindVar n cases ->
    let body = casesToTerm cases
     in E.Lam n body undefined
  LeftRight lbind lcase rbind rcase ->
    E.App
      ( E.App
          (E.Prim M.IfLeft undefined)
          (E.Lam lbind (casesToTerm lcase) undefined)
          undefined
      )
      (E.Lam rbind (casesToTerm rcase) undefined)
      undefined
  BindPair a b cases ->
    let body = casesToTerm cases
     in E.Lam
          "pair"
          ( E.App
              ( E.App
                  (E.Lam a (E.Lam b body undefined) undefined)
                  (E.App (E.Prim M.Fst undefined) (E.Var "pair" undefined) undefined)
                  undefined
              )
              (E.App (E.Prim M.Snd undefined) (E.Var "pair" undefined) undefined)
              undefined
          )
          undefined

transformDatatype :: PreGlobals -> GlobalName -> [EDataCon] -> Global
transformDatatype globals name cons =
  let valueToADT v =
        case v of
          IR.VPrimTy (M.PrimTy t) -> Prim t
          IR.VNeutral (IR.NFree (IR.Global g)) ->
            let Just v = HM.lookup g globals
             in case v of
                  -- Note that this will break on mutually recursive definitions.
                  EGDatatype n cs -> let GDatatype _ t = transformDatatype globals n cs in t
          IR.VPi _ arg res ->
            Product (valueToADT arg) (valueToADT res)
      dataconToADT (EDataCon _ ty) =
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

transformDataCon :: PreGlobals -> EDataCon -> Global
transformDataCon globals (EDataCon name ty) =
  let datatypeName = dataConTypeName ty
      Just (EGDatatype _ cons) = HM.lookup datatypeName globals
      [(_, index)] = filter ((==) name . eDataConName . fst) $ zip cons [0 ..]
      valueToTy v =
        case v of
          IR.VPrimTy (M.PrimTy t) -> E.PrimTy (M.PrimTy t)
          IR.VNeutral (IR.NFree (IR.Global g)) ->
            let Just v = HM.lookup g globals
             in case v of
                  EGDatatype d cs ->
                    let GDatatype _ adt = transformDatatype globals d cs
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
            Just (GFunction name term) -> rec (casesToTerm term)
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

indexToCons :: Int -> ADT -> [Con]
indexToCons index adt =
  case adt of
    Prim _ -> []
    Sum a b ->
      case index of
        0 -> indexToCons index a <> [LeftCon]
        n -> indexToCons (index - 1) b <> [RightCon]
    Product a b ->
      -- TODO Check this for complex nested types, will the stack operations be correct?
      indexToCons index b <> indexToCons index a <> [PairCon]

-- Note: must reverse `cons` beforehand.
consToTerm :: Int -> Type -> MU.Type -> [Con] -> Term
consToTerm n ty mty cs =
  let argName = intern ("arg_" <> show n)
   in case cs of
        [] ->
          E.Lam argName (E.Var argName ty) (E.Pi (Usage.SNat 1) ty ty)
        c : cs ->
          let E.Lam name body _ = consToTerm (n + 1) ty mty cs
           in case c of
                LeftCon -> E.Lam name (E.App (E.Prim M.Left undefined) body undefined) undefined
                RightCon -> E.Lam name (E.App (E.Prim M.Right undefined) body undefined) undefined
                PairCon -> E.Lam name (E.Lam argName (E.App (E.App (E.Prim M.Pair undefined) body undefined) (E.Var argName undefined) undefined) undefined) undefined

dataconToMichelson :: Type -> ADT -> Int -> Term
dataconToMichelson ty adt index =
  let pty = adtToMichelsonType adt
      cons = indexToCons index adt
      term = consToTerm 0 ty pty (reverse cons)
   in term
