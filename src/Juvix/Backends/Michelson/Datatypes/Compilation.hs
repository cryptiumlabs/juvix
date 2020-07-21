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
      IR.GDatatype d -> pure (transformDatatype globals d)
      IR.GDataCon c -> pure (transformDataCon globals c)
      IR.GFunction f -> pure (transformFunction globals f)
      IR.GAbstract _ _ -> Nothing

transformFunction :: PreGlobals -> IR.Function M.PrimTy M.PrimVal -> Global
transformFunction globals (IR.Function name _ ty clauses) =
  let ty' = undefined in
  -- clause each [pattern] [term]
  -- convert to nested case statements
  GFunction ty' undefined 

transformDatatype :: PreGlobals -> Datatype -> Global
transformDatatype globals (IR.Datatype name _ _ cons) =
  let valueToADT v =
        case v of
          IR.VPrimTy (M.PrimTy t) -> Prim t
          IR.VNeutral (IR.NFree (IR.Global g)) ->
            let Just v = HM.lookup g globals
             in case v of
                  -- Note that this will break on mutually recursive definitions.
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
      [(_, index)] = filter ((==) name . IR.conName . fst) $ zip cons [0 ..]
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

{-
patternsToCases :: [Pattern] -> Cases
patternsToCases [only] = OneCase only
patternsToCases [first, second] = TwoCase first second
patternsToCases (x : xs) = NestCase x (patternsToCases xs)
-}
