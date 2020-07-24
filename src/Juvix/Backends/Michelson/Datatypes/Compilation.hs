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
      GDatatype _ adt = transformDatatype globals n cs in
  case clauses of
    -- Product type.
    (EFunClause [patt] term) :| [] ->
      case patt of
        -- extract types
        -- fst/snd
        IR.PCon n [a, b] -> undefined
    -- Sum type.
    (EFunClause [pattA] termA) :| [EFunClause [pattB] termB] ->
      -- figure out which constructor is first
      --
      -- IF_LEFT {a} {b} as appropriate
      undefined
{-
  let ty' = undefined
      folded = foldClauses clauses    
      cases = toCases folded
  in GFunction ty' cases
-}

{-
foldClauses :: NonEmpty EFunClause -> PatternOrTerm
foldClauses cs = undefined

toCases :: PatternOrTerm -> Cases
toCases = undefined

casesToTerm :: Cases -> Term
casesToTerm cases =
  case cases of
    -- Single case, not a sum type.
    OneCase (b, ty, t) ->
      case b of
        NoBind -> E.Lam "_" t (E.Pi mempty ty (E.getType t))
        VarBind n -> E.Lam n t (E.Pi (Usage.SNat 1) ty (E.getType t))
        -- Simple pair.
        -- TODO extract types
        -- TODO apply fst/snd
        ConBind n [x, y] ->
          E.Lam n (E.App (
            E.Lam x (
              E.Lam y t (E.Pi (Usage.SNat 1) undefined (E.getType t))
            ) (E.Pi (Usage.SNat 1) undefined undefined)  
          ) undefined undefined) (E.Pi (Usage.SNat 2) ty (E.getType t))
    -- Two cases, sum type.
    TwoCase a b ->
      -- switch on constructor?
      undefined
-}

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
            Just (GFunction name term) -> rec term
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

patternToCases :: Pattern -> Cases
patternToCases patt =
  case patt of
    IR.PCon _ patts ->
      -- turn into de-construction followed by application
      undefined
    IR.PVar v ->
      -- turn into application of function (\v -> term)
      undefined
    IR.PDot _ ->
      -- can safely be ignored
      undefined
    IR.PPrim _ ->
      -- not supporting this for now, later turn into EQ but we need a default case
      undefined
