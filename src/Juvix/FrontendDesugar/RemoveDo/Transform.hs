module Juvix.FrontendDesugar.RemoveDo.Transform where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Juvix.FrontendDesugar.RemoveDo.Types as New
import qualified Juvix.FrontendDesugar.RemovePunned.Types as Old
import Juvix.Library

-- The actual transform we are doing

transformDo :: Old.Do -> New.Expression
transformDo (Old.Do'' body) = foldr f (transformLast last) body
  where
    f (Old.DoBody (Just name) expr) acc =
      -- we shouldn't have a prefixed name her :(
      New.MatchLogic (New.MatchName (NonEmpty.head name)) Nothing :| []
        |> flip New.Lamb (transformExpression expr)
        |> New.Lambda
        |> formApplication ">>=" acc
    f (Old.DoBody Nothing expr) acc =
      transformExpression expr
        |> formApplication ">>" acc
    formApplication name arg2 arg1 =
      New.App (New.Name (name :| [])) (arg1 :| [arg2]) |> New.Application
    --
    last =
      NonEmpty.last body
    transformLast (Old.DoBody Nothing expr) =
      transformExpression expr
    -- should we throw a warning for this
    -- issue is we have to lift every function below
    -- as this is part of expression
    transformLast (Old.DoBody (Just _) expr) =
      transformExpression expr

--------------------------------------------------------------------------------
-- Boilerplate Transforms
--------------------------------------------------------------------------------
transformtTopLevel :: Old.TopLevel -> New.TopLevel
transformtTopLevel (Old.Type t) =
  New.Type (transformType t)
transformtTopLevel (Old.ModuleOpen t) =
  New.ModuleOpen (transformModuleOpen t)
transformtTopLevel (Old.Function t) =
  New.Function (transformFunction t)
transformtTopLevel Old.TypeClass =
  New.TypeClass
transformtTopLevel Old.TypeClassInstance =
  New.TypeClassInstance

transformExpression :: Old.Expression -> New.Expression
transformExpression (Old.Constant c) =
  New.Constant (transformConst c)
transformExpression (Old.Let l) =
  New.Let (transformLet l)
transformExpression (Old.LetType l) =
  New.LetType (transformLetType l)
transformExpression (Old.Match m) =
  New.Match (transformMatch m)
transformExpression (Old.Name n) =
  New.Name n
transformExpression (Old.OpenExpr n) =
  New.OpenExpr (transformModuleOpenExpr n)
transformExpression (Old.Lambda l) =
  New.Lambda (transformLambda l)
transformExpression (Old.Application a) =
  New.Application (transformApplication a)
transformExpression (Old.Block b) =
  New.Block (transformBlock b)
transformExpression (Old.Infix i) =
  New.Infix (transformInfix i)
transformExpression (Old.ExpRecord i) =
  New.ExpRecord (transformExpRecord i)
transformExpression (Old.Do i) =
  transformDo i
transformExpression (Old.ArrowE i) =
  New.ArrowE (transformArrowExp i)
transformExpression (Old.NamedTypeE i) =
  New.NamedTypeE (transformNamedType i)
transformExpression (Old.RefinedE i) =
  New.RefinedE (transformTypeRefine i)
transformExpression (Old.UniverseName i) =
  New.UniverseName (transformUniverseExpression i)
transformExpression (Old.Parened e) =
  New.Parened (transformExpression e)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

transformType :: Old.Type -> New.Type
transformType (Old.Typ usage name' args form) =
  New.Typ (transformExpression <$> usage) name' args (transformTypeSum form)

transformTypeSum :: Old.TypeSum -> New.TypeSum
transformTypeSum (Old.Alias a) = New.Alias (transformAlias a)
transformTypeSum (Old.Data da) = New.Data (transformData da)

transformAlias :: Old.Alias -> New.Alias
transformAlias (Old.AliasDec exp) =
  New.AliasDec (transformExpression exp)

--------------------------------------------------
-- Arrows
--------------------------------------------------

transformNamedType :: Old.NamedType -> New.NamedType
transformNamedType (Old.NamedType' name exp) =
  New.NamedType' (transformName name) (transformExpression exp)

transformTypeRefine :: Old.TypeRefine -> New.TypeRefine
transformTypeRefine (Old.TypeRefine name refine) =
  New.TypeRefine (transformExpression name) (transformExpression refine)

--------------------------------------------------
-- Types Misc
--------------------------------------------------

transformName :: Old.Name -> New.Name
transformName (Old.Implicit s) = New.Implicit s
transformName (Old.Concrete s) = New.Concrete s

transformArrowSymbol :: Old.ArrowSymbol -> New.ArrowSymbol
transformArrowSymbol (Old.ArrowUse usage) =
  New.ArrowUse usage
transformArrowSymbol (Old.ArrowExp e) =
  New.ArrowExp (transformExpression e)

transformUniverseExpression ::
  Old.UniverseExpression -> New.UniverseExpression
transformUniverseExpression (Old.UniverseExpression s) =
  New.UniverseExpression s

--------------------------------------------------
-- ADTs
--------------------------------------------------

transformData :: Old.Data -> New.Data
transformData (Old.Arrowed exp adt) =
  New.Arrowed (transformExpression exp) (transformAdt adt)
transformData (Old.NonArrowed adt) =
  New.NonArrowed (transformAdt adt)

transformAdt :: Old.Adt -> New.Adt
transformAdt (Old.Sum oldsu) = New.Sum (transformSum <$> oldsu)
transformAdt (Old.Product p) = New.Product (transformProduct p)

transformSum :: Old.Sum -> New.Sum
transformSum (Old.S sym prod) =
  New.S sym (transformProduct <$> prod)

transformProduct :: Old.Product -> New.Product
transformProduct (Old.Record rec') = New.Record (transformRecord rec')
transformProduct (Old.Arrow arrow) = New.Arrow (transformExpression arrow)
transformProduct (Old.ADTLike adt) = New.ADTLike (transformExpression <$> adt)

transformRecord :: Old.Record -> New.Record
transformRecord (Old.Record'' fields sig) =
  New.Record'' (transformNameType <$> fields) (transformExpression <$> sig)

transformNameType :: Old.NameType -> New.NameType
transformNameType (Old.NameType' sig name) =
  New.NameType' (transformExpression sig) (transformName name)

transformFunction :: Old.Function -> New.Function
transformFunction (Old.Func name f sig) =
  New.Func name (transformFunctionLike <$> f) (transformSignature <$> sig)

transformFunctionLike ::
  Old.FunctionLike Old.Expression -> New.FunctionLike New.Expression
transformFunctionLike (Old.Like name args body) =
  New.Like name (transformArg <$> args) (transformExpression body)

transformModuleOpen :: Old.ModuleOpen -> New.ModuleOpen
transformModuleOpen (Old.Open mod) = New.Open mod

transformModuleOpenExpr :: Old.ModuleOpenExpr -> New.ModuleOpenExpr
transformModuleOpenExpr (Old.OpenExpress modName expr) =
  New.OpenExpress modName (transformExpression expr)

transformArg :: Old.Arg -> New.Arg
transformArg (Old.ImplicitA ml) = New.ImplicitA (transformMatchLogic ml)
transformArg (Old.ConcreteA ml) = New.ConcreteA (transformMatchLogic ml)

--------------------------------------------------------------------------------
-- Signatures
--------------------------------------------------------------------------------

transformSignature :: Old.Signature -> New.Signature
transformSignature (Old.Sig name usage arrow constraints) =
  New.Sig
    name
    (transformExpression <$> usage)
    (transformExpression arrow)
    (transformExpression <$> constraints)

--------------------------------------------------------------------------------
-- Expression
--------------------------------------------------------------------------------

transformArrowExp :: Old.ArrowExp -> New.ArrowExp
transformArrowExp (Old.Arr' left usage right) =
  New.Arr'
    (transformExpression left)
    (transformExpression usage)
    (transformExpression right)

transformConst :: Old.Constant -> New.Constant
transformConst (Old.Number numb) = New.Number (transformNumb numb)
transformConst (Old.String str) = New.String (transformString str)

transformNumb :: Old.Numb -> New.Numb
transformNumb (Old.Integer' i) = New.Integer' i
transformNumb (Old.Double' d) = New.Double' d

transformString :: Old.String' -> New.String'
transformString (Old.Sho t) = New.Sho t

transformBlock :: Old.Block -> New.Block
transformBlock (Old.Bloc expr) = New.Bloc (transformExpression expr)

transformLambda :: Old.Lambda -> New.Lambda
transformLambda (Old.Lamb args body) =
  New.Lamb (transformMatchLogic <$> args) (transformExpression body)

transformApplication :: Old.Application -> New.Application
transformApplication (Old.App fun args) =
  New.App (transformExpression fun) (transformExpression <$> args)

transformExpRecord :: Old.ExpRecord -> New.ExpRecord
transformExpRecord (Old.ExpressionRecord fields) =
  New.ExpressionRecord (transformNameSet transformExpression <$> fields)

--------------------------------------------------
-- Symbol Binding
--------------------------------------------------

transformLet :: Old.Let -> New.Let
transformLet (Old.LetGroup name bindings body) =
  New.LetGroup name (fmap transformFunctionLike bindings) (transformExpression body)

transformLetType :: Old.LetType -> New.LetType
transformLetType (Old.LetType'' typ expr) =
  New.LetType'' (transformType typ) (transformExpression expr)

--------------------------------------------------
-- Symbol Binding
--------------------------------------------------

transformInfix :: Old.Infix -> New.Infix
transformInfix (Old.Inf l o r) =
  New.Inf (transformExpression l) o (transformExpression r)

--------------------------------------------------
-- Matching
--------------------------------------------------

transformMatch :: Old.Match -> New.Match
transformMatch (Old.Match'' on bindings) =
  New.Match'' (transformExpression on) (transformMatchL <$> bindings)

transformMatchL :: Old.MatchL -> New.MatchL
transformMatchL (Old.MatchL pat body) =
  New.MatchL (transformMatchLogic pat) (transformExpression body)

transformMatchLogic :: Old.MatchLogic -> New.MatchLogic
transformMatchLogic (Old.MatchLogic start name) =
  New.MatchLogic (tranformMatchLogicStart start) name

tranformMatchLogicStart :: Old.MatchLogicStart -> New.MatchLogicStart
tranformMatchLogicStart (Old.MatchCon conName logic) =
  Old.MatchCon conName (transformMatchLogic <$> logic)
tranformMatchLogicStart (Old.MatchName s) =
  New.MatchName s
tranformMatchLogicStart (Old.MatchConst c) =
  New.MatchConst (transformConst c)
tranformMatchLogicStart (Old.MatchRecord r) =
  New.MatchRecord (transformNameSet transformMatchLogic <$> r)

transformNameSet :: (t -> t1) -> Old.NameSet t -> New.NameSet t1
transformNameSet p (Old.NonPunned s e) =
  New.NonPunned s (p e)
