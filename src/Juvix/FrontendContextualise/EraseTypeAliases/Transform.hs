module Juvix.FrontendContextualise.EraseTypeAliases.Transform where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Juvix.Core.Common.Context as Context
import qualified Juvix.FrontendContextualise.Environment as Env
import qualified Juvix.FrontendContextualise.EraseTypeAliases.Types as New
import qualified Juvix.FrontendDesugar.RemoveDo.Types as Old
import Juvix.Library

type WorkingMaps m =
  ( HasState "old" Int m, --FIXME put in the actual state sig
    HasReader "new" Int m,
    HasState "aliases" Int m
  )

-- The actual transform we are doing: erase type alias
-- TODO: write the actual transform function
{-
transformLet :: WorkingMaps m => Old.Let -> m New.Let
transformLet (Old.LetGroup name bindings body) = do
  undefined

transformLetType :: WorkingMaps m => Old.LetType -> m New.LetType
transformLetType old = do
  undefined

transformName :: WorkingMaps m => Old.Name -> m New.Name
transformName old = do
  case old of
    (Old.Implicit s) -> undefined
    (Old.Concrete s) -> undefined
-}
--------------------------------------------------------------------------------
-- Boilerplate Transforms
--------------------------------------------------------------------------------
transformTopLevel :: WorkingMaps m => Old.TopLevel -> m New.TopLevel
transformTopLevel (Old.Type t) = New.Type <$> transformType t
transformTopLevel (Old.ModuleOpen t) = New.ModuleOpen <$> transformModuleOpen t
transformTopLevel (Old.Function t) = New.Function <$> transformFunction t
transformTopLevel Old.TypeClass = pure New.TypeClass
transformTopLevel Old.TypeClassInstance = pure New.TypeClassInstance

transformExpression :: WorkingMaps m => Old.Expression -> m New.Expression
transformExpression (Old.Constant c) = New.Constant <$> transformConst c
transformExpression (Old.Let l) = New.Let <$> transformLet l
transformExpression (Old.LetType l) = New.LetType <$> transformLetType l
transformExpression (Old.Match m) = New.Match <$> transformMatch m
transformExpression (Old.Name n) = pure $ New.Name n
transformExpression (Old.OpenExpr n) = New.OpenExpr <$> transformModuleOpenExpr n
transformExpression (Old.Lambda l) = New.Lambda <$> transformLambda l
transformExpression (Old.Application a) = New.Application <$> transformApplication a
transformExpression (Old.Block b) = New.Block <$> transformBlock b
transformExpression (Old.Infix i) = New.Infix <$> transformInfix i
transformExpression (Old.ExpRecord i) = New.ExpRecord <$> transformExpRecord i
transformExpression (Old.ArrowE i) = New.ArrowE <$> transformArrowExp i
transformExpression (Old.NamedTypeE i) = New.NamedTypeE <$> transformNamedType i
transformExpression (Old.RefinedE i) = New.RefinedE <$> transformTypeRefine i
transformExpression (Old.UniverseName i) = New.UniverseName <$> transformUniverseExpression i
transformExpression (Old.Parened e) = New.Parened <$> transformExpression e

-- TODO
--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

transformType :: WorkingMaps m => Old.Type -> m New.Type
transformType (Old.Typ usage name' args form) = 
  New.Typ <$> traverse transformExpression usage <*> pure name' <*> pure args <*> transformTypeSum form

transformTypeSum :: WorkingMaps m => Old.TypeSum -> m New.TypeSum
transformTypeSum (Old.Alias a) = New.Alias <$> transformAlias a
transformTypeSum (Old.Data da) = New.Data <$> transformData da

transformAlias :: WorkingMaps m => Old.Alias -> m New.Alias
transformAlias (Old.AliasDec exp) =
  New.AliasDec <$> transformExpression exp

--------------------------------------------------
-- Arrows
--------------------------------------------------

transformNamedType :: WorkingMaps m => Old.NamedType -> m New.NamedType
transformNamedType (Old.NamedType' name exp) =
  New.NamedType' <$> transformName name <*> transformExpression exp

transformTypeRefine :: WorkingMaps m => Old.TypeRefine -> m New.TypeRefine
transformTypeRefine (Old.TypeRefine name refine) =
  New.TypeRefine <$> transformExpression name <*> transformExpression refine

--------------------------------------------------
-- Types Misc
--------------------------------------------------

transformName :: WorkingMaps m => Old.Name -> m New.Name
transformName (Old.Implicit s) = pure $ New.Implicit s
transformName (Old.Concrete s) = pure $ New.Concrete s

transformArrowSymbol :: WorkingMaps m => Old.ArrowSymbol -> m New.ArrowSymbol
transformArrowSymbol (Old.ArrowUse usage) =
  pure $ New.ArrowUse usage
transformArrowSymbol (Old.ArrowExp e) =
  New.ArrowExp <$> transformExpression e

transformUniverseExpression ::
  WorkingMaps m =>
  Old.UniverseExpression ->
  m New.UniverseExpression
transformUniverseExpression (Old.UniverseExpression s) =
  pure $ New.UniverseExpression s

--------------------------------------------------
-- ADTs
--------------------------------------------------

transformData :: WorkingMaps m => Old.Data -> m New.Data
transformData (Old.Arrowed exp adt) =
  New.Arrowed <$> transformExpression exp <*> transformAdt adt
transformData (Old.NonArrowed adt) =
  New.NonArrowed <$> transformAdt adt

transformAdt :: WorkingMaps m => Old.Adt -> m New.Adt
transformAdt (Old.Sum oldsu) = New.Sum <$> traverse transformSum oldsu
transformAdt (Old.Product p) = New.Product <$> transformProduct p

transformSum :: WorkingMaps m => Old.Sum -> m New.Sum
transformSum (Old.S sym prod) =
  New.S <$> pure sym <*> traverse transformProduct prod

transformProduct :: WorkingMaps m => Old.Product -> m New.Product
transformProduct (Old.Record rec') = New.Record <$> transformRecord rec'
transformProduct (Old.Arrow arrow) = New.Arrow <$> transformExpression arrow
transformProduct (Old.ADTLike adt) = New.ADTLike <$> traverse transformExpression adt

transformRecord :: WorkingMaps m => Old.Record -> m New.Record
transformRecord (Old.Record'' fields sig) =
  New.Record'' <$> traverse transformNameType fields <*> traverse transformExpression sig

transformNameType :: WorkingMaps m => Old.NameType -> m New.NameType
transformNameType (Old.NameType' sig name) =
  New.NameType' <$> transformExpression sig <*> transformName name

transformFunction :: WorkingMaps m => Old.Function -> m New.Function
transformFunction (Old.Func name f sig) =
  New.Func <$> pure name <*> traverse transformFunctionLike f <*> traverse transformSignature sig

transformFunctionLike ::
  WorkingMaps m =>
  Old.FunctionLike Old.Expression ->
  m (New.FunctionLike New.Expression)
transformFunctionLike (Old.Like args body) =
  New.Like <$> traverse transformArg args <*> transformExpression body

transformModuleOpen :: WorkingMaps m => Old.ModuleOpen -> m New.ModuleOpen
transformModuleOpen (Old.Open mod) = pure $ New.Open mod

transformModuleOpenExpr :: WorkingMaps m => Old.ModuleOpenExpr -> m New.ModuleOpenExpr
transformModuleOpenExpr (Old.OpenExpress modName expr) =
  New.OpenExpress <$> pure modName <*> transformExpression expr

transformArg :: WorkingMaps m => Old.Arg -> m New.Arg
transformArg (Old.ImplicitA ml) = New.ImplicitA <$> transformMatchLogic ml
transformArg (Old.ConcreteA ml) = New.ConcreteA <$> transformMatchLogic ml

--------------------------------------------------------------------------------
-- Signatures
--------------------------------------------------------------------------------

transformSignature :: WorkingMaps m => Old.Signature -> m New.Signature
transformSignature (Old.Sig name usage arrow constraints) =
  New.Sig
    <$> pure name
    <*> traverse transformExpression usage
    <*> transformExpression arrow
    <*> traverse transformExpression constraints

--------------------------------------------------------------------------------
-- Expression
--------------------------------------------------------------------------------

transformArrowExp :: WorkingMaps m => Old.ArrowExp -> m New.ArrowExp
transformArrowExp (Old.Arr' left usage right) =
  New.Arr'
    <$> transformExpression left
    <*> transformExpression usage
    <*> transformExpression right

transformConst :: WorkingMaps m => Old.Constant -> m New.Constant
transformConst (Old.Number numb) = New.Number <$> transformNumb numb
transformConst (Old.String str) = New.String <$> transformString str

transformNumb :: WorkingMaps m => Old.Numb -> m New.Numb
transformNumb (Old.Integer' i) = pure $ New.Integer' i
transformNumb (Old.Double' d) = pure $ New.Double' d

transformString :: WorkingMaps m => Old.String' -> m New.String'
transformString (Old.Sho t) = pure $ New.Sho t

transformBlock :: WorkingMaps m => Old.Block -> m New.Block
transformBlock (Old.Bloc expr) = New.Bloc <$> transformExpression expr

transformLambda :: WorkingMaps m => Old.Lambda -> m New.Lambda
transformLambda (Old.Lamb args body) =
  New.Lamb <$> traverse transformMatchLogic args <*> transformExpression body

transformApplication :: WorkingMaps m => Old.Application -> m New.Application
transformApplication (Old.App fun args) =
  New.App <$> transformExpression fun <*> traverse transformExpression args

transformExpRecord :: WorkingMaps m => Old.ExpRecord -> m New.ExpRecord
transformExpRecord (Old.ExpressionRecord fields) =
  New.ExpressionRecord <$> (transformNameSet transformExpression <$> fields)

--------------------------------------------------
-- Symbol Binding
--------------------------------------------------

transformLet :: WorkingMaps m => Old.Let -> m New.Let
transformLet (Old.LetGroup name bindings body) =
  New.LetGroup 
    <$> pure name 
    <*> traverse transformFunctionLike bindings
    <*> transformExpression body

transformLetType :: WorkingMaps m => Old.LetType -> m New.LetType
transformLetType (Old.LetType'' typ expr) =
  New.LetType'' <$> transformType typ <*> transformExpression expr

--------------------------------------------------
-- Symbol Binding
--------------------------------------------------

transformInfix :: WorkingMaps m => Old.Infix -> m New.Infix
transformInfix (Old.Inf l o r) =
  New.Inf <$> transformExpression l <*> pure o <*> transformExpression r

--------------------------------------------------
-- Matching
--------------------------------------------------

transformMatch :: WorkingMaps m => Old.Match -> m New.Match
transformMatch (Old.Match'' on bindings) =
  New.Match'' <$> transformExpression on <*> traverse transformMatchL bindings

transformMatchL :: WorkingMaps m => Old.MatchL -> m New.MatchL
transformMatchL (Old.MatchL pat body) =
  New.MatchL <$> transformMatchLogic pat <*> transformExpression body

transformMatchLogic :: WorkingMaps m => Old.MatchLogic -> m New.MatchLogic
transformMatchLogic (Old.MatchLogic start name) =
  New.MatchLogic <$> tranformMatchLogicStart start <*> pure name

tranformMatchLogicStart :: WorkingMaps m => Old.MatchLogicStart -> m New.MatchLogicStart
tranformMatchLogicStart (Old.MatchCon conName logic) =
  Old.MatchCon <$> pure conName <*> traverse transformMatchLogic logic
tranformMatchLogicStart (Old.MatchName s) =
  New.MatchName s
tranformMatchLogicStart (Old.MatchConst c) =
  New.MatchConst <$> transformConst c
tranformMatchLogicStart (Old.MatchRecord r) =
  --New.MatchRecord <$> (transformNameSet transformMatchLogic <$> r)
  New.MatchRecord 
    <$> transformNameSet 
      (New.matchLogicContents (traverse transformMatchLogic r)) 
      (New.matchLogicNamed (traverse transformMatchLogic r))

transformNameSet :: WorkingMaps m => (t -> t1) -> Old.NameSet t -> m (New.NameSet t1)
transformNameSet p (Old.NonPunned s e) =
  New.NonPunned <$> pure s <*> pure (p e)
