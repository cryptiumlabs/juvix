module Juvix.FrontendContextualise.EraseTypeAliases.Transform where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Juvix.Core.Common.Context as Context
import qualified Juvix.FrontendContextualise.Environment as Env
import qualified Juvix.FrontendContextualise.EraseTypeAliases.Types as New
import qualified Juvix.FrontendDesugar.RemoveDo.Types as Old
import Juvix.Library

type WorkingMaps m =  
  ( HasState "old" (Context.T term0 ty0 sumRep0) m,
    HasReader "new" (Context.T termN tyN sumRepN) m,
    HasState  "aliases" (Map Name Expression) m
  ) 

-- The actual transform we are doing: erase type alias
-- TODO: write the actual transform function

transformLet :: WorkingMaps m => m Old.Let -> m New.Let
transformLet mOld = do
  (Old.LetGroup name bindings body) <- mOld
  undefined
transformLetType :: WorkingMaps m => m Old.LetType -> m New.LetType
transformLetType mOld = do
  (Old.LetGroup name bindings body) <- mOld
  undefined
transformName :: WorkingMaps m => m Old.Name -> m New.Name
transformName mOld = do
  oldName <- mOld
  case oldName of
    (Old.Implicit s) -> undefined
    (Old.Concrete s) -> undefined

--------------------------------------------------------------------------------
-- Boilerplate Transforms
--------------------------------------------------------------------------------
transformTopLevel :: WorkingMaps m => m Old.TopLevel -> m New.TopLevel
transformTopLevel mOld = do
  oldTopLevel <- mOld
  case oldTopLevel of
    (Old.Type t) -> pure $ New.Type (transformType t)
    (Old.ModuleOpen t) -> pure $ New.ModuleOpen (transformModuleOpen t)
    (Old.Function t) -> pure $ New.Function (transformFunction t)
    Old.TypeClass -> pure New.TypeClass
    Old.TypeClassInstance -> pure New.TypeClassInstance

transformExpression :: WorkingMaps m => m Old.Expression -> m New.Expression
transformExpression mOld = do
  oldExpression <- mOld
  case oldExpression of 
    (Old.Constant c) -> pure $ New.Constant (transformConst c)
    (Old.Let l) -> pure $ New.Let (transformLet l)
    (Old.LetType l) -> pure $ New.LetType (transformLetType l)
    (Old.Match m) -> pure $ New.Match (transformMatch m)
    (Old.Name n) -> pure $ New.Name n
    (Old.OpenExpr n) -> pure $ New.OpenExpr (transformModuleOpenExpr n)
    (Old.Lambda l) -> pure $ New.Lambda (transformLambda l)
    (Old.Application a) -> pure $ New.Application (transformApplication a)
    (Old.Block b) -> pure $ New.Block (transformBlock b)
    (Old.Infix i) -> pure $ New.Infix (transformInfix i)
    (Old.ExpRecord i) -> pure $ New.ExpRecord (transformExpRecord i)
    (Old.ArrowE i) -> pure $ New.ArrowE (transformArrowExp i)
    (Old.NamedTypeE i) -> pure $ New.NamedTypeE (transformNamedType i)
    (Old.RefinedE i) -> pure $ New.RefinedE (transformTypeRefine i)
    (Old.UniverseName i) -> pure $ New.UniverseName (transformUniverseExpression i)
    (Old.Parened e) -> pure $ New.Parened (transformExpression e)
-- TODO
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
transformFunctionLike (Old.Like args body) =
  New.Like (transformArg <$> args) (transformExpression body)

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
