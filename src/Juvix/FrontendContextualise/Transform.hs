module Juvix.FrontendContextualise.Transform where

import Juvix.Library
import qualified Juvix.Core.Common.Context as Context
import qualified Juvix.FrontendDesugar.RemoveDo.Types as Base
import qualified Juvix.FrontendContextualise.Environment as Env
import qualified Juvix.Core.Usage as Usage

transformTopLevel :: Base.TopLevel
                   -> Env.Context term0 ty0 sumRep0 termN tyN sumRepN Base.TopLevel
transformTopLevel (Base.Type type'()) =
  Base.Type <$> transformType type'
transformTopLevel (Base.ModuleOpen mod) =
  return (Base.ModuleOpen mod)
transformTopLevel (Base.Function f) =
  Base.Function <$> transformFunction f
transformTopLevel Base.TypeClass =
  return Base.TypeClass
transformTopLevel Base.TypeClassInstance =
  return Base.TypeClassInstance

transformFunction :: Base.Function
                  -> Env.Context term0 ty0 sumRep0 termN tyN sumRepN Base.Function
transformFunction (Base.Func name f sig) =
  do
    () <- Env.add name (Context.Def { definitionUsage = Usage.Omega
                                    -- What should be saved here?
                                    , definitionTy = Nothing
                                    , definitionTerm = Nothing
                                    , precedence = Context.default'
                                    }
                 )
    pure Base.Func <*> pure name <*> traverse transformFunctionLike f <*> traverse transformSignature sig

transformExpression :: Base.Expression
                    -> Env.Context term0 ty0 sumRep0 termN tyN sumRepN Base.Expression
transformExpression (Base.Constant c) =
  return (Base.Constant c)
transformExpression (Base.Let l) =
  Base.Let <$> transformLet l
transformExpression (Base.LetType l) =
  return (Base.LetType l)
transformExpression (Base.Match match) =
  return (Base.Match match)
transformExpression (Base.Name name) =
  return (Base.Name name)
transformExpression (Base.OpenExpr n) =
  Base.OpenExpr <$> transformModuleOpenExpr n
transformExpression (Base.Lambda l) =
  Base.Lambda <$> transformLambda l
transformExpression (Base.Application a) =
  Base.Application <$> transformApplication a
transformExpression (Base.Block b) =
  Base.Block <$> transformBlock b
transformExpression (Base.Infix i) =
  Base.Infix <$> transformInfix i
transformExpression (Base.ExpRecord i) =
  Base.ExpRecord <$> transformExpRecord i
transformExpression (Base.ArrowE i) =
  Base.ArrowE <$> transformArrowExp i
transformExpression (Base.NamedTypeE i) =
  Base.NamedTypeE <$> transformNamedType i
transformExpression (Base.RefinedE i) =
  Base.RefinedE <$> transformTypeRefine i
transformExpression (Base.UniverseName i) =
  Base.UniverseName <$> transformUniverseExpression i
transformExpression (Base.Parened e) =
  Base.Parened <$> transformExpression e

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

transformType :: Base.Type -> Env.Context term0 ty0 sumRep0 termN tyN sumRepN Base.Type
transformType (Base.Typ usage name' args form) =
    pure Base.Typ <*> traverse transformExpression usage <*> pure name' <*> pure args <*> transformTypeSum form

transformTypeSum :: Base.TypeSum -> Env.Context term0 ty0 sumRep0 termN tyN sumRepN Base.TypeSum
transformTypeSum (Base.Alias a) =
  Base.Alias <$> transformAlias a
transformTypeSum (Base.Data da) =
  Base.Data <$> transformData da

transformAlias :: Base.Alias -> Env.Context term0 ty0 sumRep0 termN tyN sumRepN Base.Alias
transformAlias (Base.AliasDec exp) =
  Base.AliasDec <$> transformExpression exp

--------------------------------------------------
-- Arrows
--------------------------------------------------

transformNamedType :: Base.NamedType -> Env.Context term0 ty0 sumRep0 termN tyN sumRepN Base.NamedType
transformNamedType (Base.NamedType' name exp) =
  pure Base.NamedType' <*> transformName name <*> transformExpression exp

transformTypeRefine :: Base.TypeRefine -> Env.Context term0 ty0 sumRep0 termN tyN sumRepN Base.TypeRefine
transformTypeRefine (Base.TypeRefine name refine) =
  pure Base.TypeRefine <*> transformExpression name <*> transformExpression refine

--------------------------------------------------
-- Types Misc
--------------------------------------------------

transformName :: Base.Name -> Env.Context term0 ty0 sumRep0 termN tyN sumRepN Base.Name
transformName (Base.Implicit s) = return (Base.Implicit s)
transformName (Base.Concrete s) = return (Base.Concrete s)

transformArrowSymbol :: Base.ArrowSymbol -> Env.Context term0 ty0 sumRep0 termN tyN sumRepN Base.ArrowSymbol
transformArrowSymbol (Base.ArrowUse usage) =
  return (Base.ArrowUse usage)
transformArrowSymbol (Base.ArrowExp e) =
  Base.ArrowExp <$> transformExpression e

transformUniverseExpression ::
   Base.UniverseExpression -> Env.Context term0 ty0 sumRep0 termN tyN sumRepN Base.UniverseExpression
transformUniverseExpression (Base.UniverseExpression s) =
  return (Base.UniverseExpression s)

--------------------------------------------------
-- ADTs
--------------------------------------------------

transformData :: Base.Data -> Env.Context term0 ty0 sumRep0 termN tyN sumRepN Base.Data
transformData (Base.Arrowed exp adt) =
  pure Base.Arrowed <*> transformExpression exp <*> transformAdt adt
transformData (Base.NonArrowed adt) =
  Base.NonArrowed <$> transformAdt adt

transformAdt :: Base.Adt -> Env.Context term0 ty0 sumRep0 termN tyN sumRepN Base.Adt
transformAdt (Base.Sum oldsu) = pure Base.Sum <*> traverse transformSum oldsu
transformAdt (Base.Product p) = Base.Product <$> transformProduct p

transformSum :: Base.Sum -> Env.Context term0 ty0 sumRep0 termN tyN sumRepN Base.Sum
transformSum (Base.S sym prod) =
  pure (Base.S sym) <*> traverse transformProduct prod

transformProduct :: Base.Product -> Env.Context term0 ty0 sumRep0 termN tyN sumRepN Base.Product
transformProduct (Base.Record rec') =
  Base.Record <$> transformRecord rec'
transformProduct (Base.Arrow arrow) =
  Base.Arrow <$> transformExpression arrow
transformProduct (Base.ADTLike adt) =
  pure Base.ADTLike <*> traverse transformExpression adt

transformRecord :: Base.Record -> Env.Context term0 ty0 sumRep0 termN tyN sumRepN Base.Record
transformRecord (Base.Record'' fields sig) =
  pure Base.Record'' <*> traverse transformNameType fields <*> traverse transformExpression sig

transformNameType :: Base.NameType -> Env.Context term0 ty0 sumRep0 termN tyN sumRepN Base.NameType
transformNameType (Base.NameType' sig name) =
  pure Base.NameType' <*> transformExpression sig <*> transformName name

transformFunctionLike ::
   Base.FunctionLike Base.Expression
   -> Env.Context term0 ty0 sumRep0 termN tyN sumRepN (Base.FunctionLike Base.Expression)
transformFunctionLike (Base.Like args body) =
  pure Base.Like <*> traverse transformArg args <*> transformExpression body

transformModuleOpen :: Base.ModuleOpen -> Env.Context term0 ty0 sumRep0 termN tyN sumRepN Base.ModuleOpen
transformModuleOpen (Base.Open mod) =
  return (Base.Open mod)

transformModuleOpenExpr :: Base.ModuleOpenExpr -> Env.Context term0 ty0 sumRep0 termN tyN sumRepN Base.ModuleOpenExpr
transformModuleOpenExpr (Base.OpenExpress modName expr) =
  pure (Base.OpenExpress modName) <*> transformExpression expr

transformArg :: Base.Arg -> Env.Context term0 ty0 sumRep0 termN tyN sumRepN Base.Arg
transformArg (Base.ImplicitA ml) =
  Base.ImplicitA <$> transformMatchLogic ml
transformArg (Base.ConcreteA ml) =
  Base.ConcreteA <$> transformMatchLogic ml

--------------------------------------------------------------------------------
-- Signatures
--------------------------------------------------------------------------------

transformSignature :: Base.Signature -> Env.Context term0 ty0 sumRep0 termN tyN sumRepN Base.Signature
transformSignature (Base.Sig name usage arrow constraints) =
  pure Base.Sig <*>
   pure name <*>
   traverse transformExpression usage <*>
   transformExpression arrow <*>
   traverse transformExpression constraints

--------------------------------------------------------------------------------
-- Expression
--------------------------------------------------------------------------------

transformArrowExp :: Base.ArrowExp -> Env.Context term0 ty0 sumRep0 termN tyN sumRepN Base.ArrowExp
transformArrowExp (Base.Arr' left usage right) =
  pure Base.Arr' <*>
    transformExpression left <*>
    transformExpression usage <*>
    transformExpression right

transformConst :: Base.Constant -> Env.Context term0 ty0 sumRep0 termN tyN sumRepN Base.Constant
transformConst (Base.Number numb) =
  Base.Number <$> transformNumb numb
transformConst (Base.String str) =
  Base.String  <$> transformString str

transformNumb :: Base.Numb -> Env.Context term0 ty0 sumRep0 termN tyN sumRepN Base.Numb
transformNumb (Base.Integer' i) =
  return (Base.Integer' i)
transformNumb (Base.Double' d) =
  return (Base.Double' d)

-- transformString :: Base.String' -> Base.String'
transformString (Base.Sho t) =
  return (Base.Sho t)

transformBlock :: Base.Block -> Env.Context term0 ty0 sumRep0 termN tyN sumRepN Base.Block
transformBlock (Base.Bloc expr) =
  Base.Bloc <$> transformExpression expr

transformLambda :: Base.Lambda -> Env.Context term0 ty0 sumRep0 termN tyN sumRepN Base.Lambda
transformLambda (Base.Lamb args body) =
  pure Base.Lamb <*> traverse transformMatchLogic args <*> transformExpression body

transformApplication :: Base.Application -> Env.Context term0 ty0 sumRep0 termN tyN sumRepN Base.Application
transformApplication (Base.App fun args) =
  pure Base.App <*> transformExpression fun <*> traverse transformExpression args

-- transformExpRecord :: Base.ExpRecord -> Base.ExpRecord
transformExpRecord (Base.ExpressionRecord fields) =
  pure Base.ExpressionRecord <*> traverse (transformNameSet transformExpression) fields

--------------------------------------------------
-- Symbol Binding
--------------------------------------------------

transformLet :: Base.Let -> Env.Context term0 ty0 sumRep0 termN tyN sumRepN Base.Let
transformLet (Base.LetGroup name bindings body) =
  pure (Base.LetGroup name) <*> traverse transformFunctionLike bindings <*> transformExpression body

transformLetType :: Base.LetType -> Env.Context term0 ty0 sumRep0 termN tyN sumRepN Base.LetType
transformLetType (Base.LetType'' typ expr) =
  pure Base.LetType'' <*> transformType typ <*> transformExpression expr

--------------------------------------------------
-- Symbol Binding
--------------------------------------------------

transformInfix :: Base.Infix -> Env.Context term0 ty0 sumRep0 termN tyN sumRepN Base.Infix
transformInfix (Base.Inf l o r) =
  pure Base.Inf <*> transformExpression l <*> pure o <*> transformExpression r

--------------------------------------------------
-- Matching
--------------------------------------------------

transformMatch :: Base.Match -> Env.Context term0 ty0 sumRep0 termN tyN sumRepN Base.Match
transformMatch (Base.Match'' on bindings) =
  pure Base.Match'' <*> transformExpression on <*> traverse transformMatchL bindings

transformMatchL :: Base.MatchL -> Env.Context term0 ty0 sumRep0 termN tyN sumRepN Base.MatchL
transformMatchL (Base.MatchL pat body) =
  pure Base.MatchL <*> transformMatchLogic pat <*> transformExpression body

transformMatchLogic :: Base.MatchLogic -> Env.Context term0 ty0 sumRep0 termN tyN sumRepN Base.MatchLogic
transformMatchLogic (Base.MatchLogic start name) =
  pure Base.MatchLogic <*> tranformMatchLogicStart start <*> pure name

tranformMatchLogicStart :: Base.MatchLogicStart -> Env.Context term0 ty0 sumRep0 termN tyN sumRepN Base.MatchLogicStart
tranformMatchLogicStart (Base.MatchCon conName logic) =
  pure (Base.MatchCon conName) <*> traverse transformMatchLogic logic
tranformMatchLogicStart (Base.MatchName s) =
  return (Base.MatchName s)
tranformMatchLogicStart (Base.MatchConst c) =
  Base.MatchConst <$> transformConst c
tranformMatchLogicStart (Base.MatchRecord r) =
  pure Base.MatchRecord <*> traverse (transformNameSet transformMatchLogic) r

transformNameSet :: (t -> Env.Context term0 ty0 sumRep0 termN tyN sumRepN t1)
                 -> Base.NameSet t
                 -> Env.Context term0 ty0 sumRep0 termN tyN sumRepN (Base.NameSet t1)
transformNameSet p (Base.NonPunned s e) =
  pure (Base.NonPunned s) <*> p e
