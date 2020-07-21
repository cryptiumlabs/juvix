{-# LANGUAGE LiberalTypeSynonyms #-}

module Juvix.FrontendContextualise.EraseTypeAliases.Transform where

--FIXME put in the last stage

import qualified Data.List.NonEmpty as NonEmpty
import qualified Juvix.Core.Common.Context as Context
import qualified Juvix.FrontendContextualise.Environment as Env
import qualified Juvix.FrontendContextualise.EraseTypeAliases.Types as New
import qualified Juvix.FrontendDesugar.RemoveDo.Types as Old
import Juvix.Library

type Old f =
  f (NonEmpty (Old.FunctionLike Old.Expression)) Old.Signature Old.Type

type New f =
  f (NonEmpty (New.FunctionLike New.Expression)) New.Signature New.Type

type WorkingMaps m =
  ( HasState "old" (Old Context.T) m, -- old context
    HasState "new" (New Context.T) m
  )

--------------------------------------------------------------------------------
-- Actual Transforms
--------------------------------------------------------------------------------

-- TODO: write the actual transform function

--------------------------------------------------------------------------------
-- Boilerplate Transforms
--------------------------------------------------------------------------------

transformContext ::
  ( HasState "new" (Context.T term ty sumRep) (StateT s Identity),
    HasState "old" (Context.T term ty sumRep) (StateT s Identity)
  ) =>
  s ->
  s
transformContext = execState transformC

transformC ::
  (Env.HasNew term ty sumRep m, Env.HasOld term ty sumRep m) => m ()
transformC = do
  old <- get @"old"
  let oldC = Context.toList old
  case oldC of
    (sym, def) : _ -> do
      Env.add sym def
      Env.removeOld sym
    [] -> pure ()

-- transformContext :: Old Context.T ->  New Context.T
-- transformContext oldMap =
--   let oldSymList = fst $ unzip $ Context.toList oldMap
--   in
--     transformContextAcc oldMap oldSymList

-- transformContextAcc :: Old Context.T -> [Symbol] -> New Context.T
-- transformContextAcc oldMap symList =
--     case symList of
--         hd : tl -> do
--           removeOldFillNew hd
--           transformContextAcc oldMap tl
--         [] -> undefined

transformTopLevel ::
  WorkingMaps m => Old.TopLevel -> m New.TopLevel
transformTopLevel (Old.Type t) = New.Type <$> transformType t
transformTopLevel (Old.ModuleOpen t) = New.ModuleOpen <$> transformModuleOpen t
transformTopLevel (Old.Function t) = New.Function <$> transformFunction t
transformTopLevel Old.TypeClass = pure New.TypeClass
transformTopLevel Old.TypeClassInstance = pure New.TypeClassInstance

transformExpression ::
  WorkingMaps m => Old.Expression -> m New.Expression
transformExpression (Old.Constant c) = New.Constant <$> transformConst c
transformExpression (Old.Let l) = New.Let <$> transformLet l
transformExpression (Old.LetType l) = New.LetType <$> transformLetType l
transformExpression (Old.Match m) = New.Match <$> transformMatch m
transformExpression (Old.Name n) = pure $ New.Name n
transformExpression (Old.OpenExpr n) =
  New.OpenExpr <$> transformModuleOpenExpr n
transformExpression (Old.Lambda l) = New.Lambda <$> transformLambda l
transformExpression (Old.Application a) =
  New.Application <$> transformApplication a
transformExpression (Old.Block b) = New.Block <$> transformBlock b
transformExpression (Old.Infix i) = New.Infix <$> transformInfix i
transformExpression (Old.ExpRecord i) = New.ExpRecord <$> transformExpRecord i
transformExpression (Old.ArrowE i) = New.ArrowE <$> transformArrowExp i
transformExpression (Old.NamedTypeE i) =
  New.NamedTypeE <$> transformNamedType i
transformExpression (Old.RefinedE i) = New.RefinedE <$> transformTypeRefine i
transformExpression (Old.UniverseName i) =
  New.UniverseName <$> transformUniverseExpression i
transformExpression (Old.Parened e) = New.Parened <$> transformExpression e

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

transformType ::
  WorkingMaps m => Old.Type -> m New.Type
transformType (Old.Typ usage name' args form) =
  New.Typ
    <$> traverse transformExpression usage
    <*> pure name'
    <*> pure args
    <*> transformTypeSum form

transformTypeSum ::
  WorkingMaps m => Old.TypeSum -> m New.TypeSum
transformTypeSum (Old.Alias a) = New.Alias <$> transformAlias a
transformTypeSum (Old.Data da) = New.Data <$> transformData da

transformAlias ::
  WorkingMaps m => Old.Alias -> m New.Alias
transformAlias (Old.AliasDec exp) =
  New.AliasDec <$> transformExpression exp

--------------------------------------------------
-- Arrows
--------------------------------------------------

transformNamedType ::
  WorkingMaps m => Old.NamedType -> m New.NamedType
transformNamedType (Old.NamedType' name exp) =
  New.NamedType' <$> transformName name <*> transformExpression exp

transformTypeRefine ::
  WorkingMaps m => Old.TypeRefine -> m New.TypeRefine
transformTypeRefine (Old.TypeRefine name refine) =
  New.TypeRefine <$> transformExpression name <*> transformExpression refine

--------------------------------------------------
-- Types Misc
--------------------------------------------------

transformName ::
  WorkingMaps m => Old.Name -> m New.Name
transformName (Old.Implicit s) = pure $ New.Implicit s
transformName (Old.Concrete s) = pure $ New.Concrete s

transformArrowSymbol ::
  WorkingMaps m => Old.ArrowSymbol -> m New.ArrowSymbol
transformArrowSymbol (Old.ArrowUse usage) =
  pure $ New.ArrowUse usage
transformArrowSymbol (Old.ArrowExp e) =
  New.ArrowExp <$> transformExpression e

transformUniverseExpression ::
  WorkingMaps m => Old.UniverseExpression -> m New.UniverseExpression
transformUniverseExpression (Old.UniverseExpression s) =
  pure $ New.UniverseExpression s

--------------------------------------------------
-- ADTs
--------------------------------------------------

transformData ::
  WorkingMaps m => Old.Data -> m New.Data
transformData (Old.Arrowed exp adt) =
  New.Arrowed <$> transformExpression exp <*> transformAdt adt
transformData (Old.NonArrowed adt) =
  New.NonArrowed <$> transformAdt adt

transformAdt ::
  WorkingMaps m => Old.Adt -> m New.Adt
transformAdt (Old.Sum oldsu) = New.Sum <$> traverse transformSum oldsu
transformAdt (Old.Product p) = New.Product <$> transformProduct p

transformSum ::
  WorkingMaps m => Old.Sum -> m New.Sum
transformSum (Old.S sym prod) =
  New.S sym <$> traverse transformProduct prod

transformProduct ::
  WorkingMaps m => Old.Product -> m New.Product
transformProduct (Old.Record rec') = New.Record <$> transformRecord rec'
transformProduct (Old.Arrow arrow) = New.Arrow <$> transformExpression arrow
transformProduct (Old.ADTLike adt) = New.ADTLike <$> traverse transformExpression adt

transformRecord ::
  WorkingMaps m => Old.Record -> m New.Record
transformRecord (Old.Record'' fields sig) =
  New.Record'' <$> traverse transformNameType fields <*> traverse transformExpression sig

transformNameType ::
  WorkingMaps m => Old.NameType -> m New.NameType
transformNameType (Old.NameType' sig name) =
  New.NameType' <$> transformExpression sig <*> transformName name

transformFunction ::
  WorkingMaps m => Old.Function -> m New.Function
transformFunction (Old.Func name f sig) =
  New.Func name <$> traverse transformFunctionLike f <*> traverse transformSignature sig

argLogic :: Old.Arg -> Old.MatchLogic
argLogic (Old.ConcreteA x) = x
argLogic (Old.ImplicitA x) = x

accBindings :: [Old.Arg] -> [Symbol]
accBindings = concatMap (findBindings . argLogic)

transformFunctionLike ::
  WorkingMaps m => Old.FunctionLike Old.Expression -> m (New.FunctionLike New.Expression)
transformFunctionLike (Old.Like args body) = do
  let bindings = accBindings args
  originalBindings <- traverse saveOld bindings
  transArgs <- traverse transformArg args
  --
  traverse_ Env.addUnknown bindings
  --
  res <- New.Like transArgs <$> transformExpression body
  traverse_ restoreName originalBindings
  pure res

transformModuleOpen ::
  WorkingMaps m => Old.ModuleOpen -> m New.ModuleOpen
transformModuleOpen (Old.Open mod) = pure $ New.Open mod

transformModuleOpenExpr ::
  WorkingMaps m => Old.ModuleOpenExpr -> m New.ModuleOpenExpr
transformModuleOpenExpr (Old.OpenExpress modName expr) =
  New.OpenExpress modName <$> transformExpression expr

transformArg ::
  WorkingMaps m => Old.Arg -> m New.Arg
transformArg (Old.ConcreteA ml) = New.ConcreteA <$> transformMatchLogic ml
transformArg (Old.ImplicitA ml) = New.ImplicitA <$> transformMatchLogic ml

--------------------------------------------------------------------------------
-- Signatures
--------------------------------------------------------------------------------

transformSignature ::
  WorkingMaps m => Old.Signature -> m New.Signature
transformSignature (Old.Sig name usage arrow constraints) =
  New.Sig name
    <$> traverse transformExpression usage
    <*> transformExpression arrow
    <*> traverse transformExpression constraints

--------------------------------------------------------------------------------
-- Expression
--------------------------------------------------------------------------------

transformArrowExp ::
  WorkingMaps m => Old.ArrowExp -> m New.ArrowExp
transformArrowExp (Old.Arr' left usage right) =
  New.Arr'
    <$> transformExpression left
    <*> transformExpression usage
    <*> transformExpression right

transformConst ::
  WorkingMaps m => Old.Constant -> m New.Constant
transformConst (Old.Number numb) = New.Number <$> transformNumb numb
transformConst (Old.String str) = New.String <$> transformString str

transformNumb ::
  WorkingMaps m => Old.Numb -> m New.Numb
transformNumb (Old.Integer' i) = pure $ New.Integer' i
transformNumb (Old.Double' d) = pure $ New.Double' d

transformString ::
  WorkingMaps m => Old.String' -> m New.String'
transformString (Old.Sho t) = pure $ New.Sho t

transformBlock ::
  WorkingMaps m => Old.Block -> m New.Block
transformBlock (Old.Bloc expr) = New.Bloc <$> transformExpression expr

saveOld ::
  HasState "new" (Context.T term ty sumRep) f =>
  Symbol ->
  f (Maybe (Context.Definition term ty sumRep), Symbol)
saveOld sym =
  flip (,) sym <$> Env.lookup sym

restoreName ::
  HasState "new" (Context.T term ty sumRep) m =>
  (Maybe (Context.Definition term ty sumRep), Symbol) ->
  m ()
restoreName (Just def, sym) = Env.add sym def
restoreName (Nothing, sym) = Env.remove sym

transformLambda ::
  WorkingMaps m => Old.Lambda -> m New.Lambda
transformLambda (Old.Lamb args body) = do
  let bindings = findBindings (NonEmpty.head args)
  originalBindings <- traverse saveOld bindings
  transArgs <- transformMatchLogic (NonEmpty.head args)
  --
  traverse_ Env.addUnknown bindings
  --
  res <- New.Lamb (pure transArgs) <$> transformExpression body
  traverse_ restoreName originalBindings
  pure res

transformApplication ::
  WorkingMaps m => Old.Application -> m New.Application
transformApplication (Old.App fun args) =
  New.App <$> transformExpression fun <*> traverse transformExpression args

transformExpRecord ::
  WorkingMaps m => Old.ExpRecord -> m New.ExpRecord
transformExpRecord (Old.ExpressionRecord fields) =
  New.ExpressionRecord <$> traverse (transformNameSet transformExpression) fields

--------------------------------------------------
-- Symbol Binding
--------------------------------------------------

transformLet :: WorkingMaps m => Old.Let -> m New.Let
transformLet (Old.LetGroup name bindings body) = do
  originalVal <- Env.lookup name -- look up in "new" state
  let transform = do
        transformedBindings <- traverse transformFunctionLike bindings
        let def = Env.transLike transformedBindings Nothing Nothing
        Env.add name def -- add to new context
        New.LetGroup name transformedBindings <$> transformExpression body
  case originalVal of
    Just originalV -> do
      res <- transform
      Env.add name originalV
      return res
    Nothing -> do
      res <- transform
      Env.remove name
      return res

transformLetType ::
  WorkingMaps m => Old.LetType -> m New.LetType
transformLetType (Old.LetType'' typ expr) = do
  let typeName = Old.typeName' typ
  originalVal <- Env.lookup typeName
  let transform = do
        transformedType <- transformType typ
        let def = Env.transLike transformedType Nothing Nothing
        Env.add typeName def -- add to new context
        New.LetType'' transformedType <$> transformExpression expr
  case originalVal of
    Just originalV -> do
      res <- transform
      Env.add typeName originalV
      return res
    Nothing -> do
      res <- transform
      Env.remove typeName
      return res

--------------------------------------------------
-- Symbol Binding
--------------------------------------------------

transformInfix ::
  WorkingMaps m => Old.Infix -> m New.Infix
transformInfix (Old.Inf l o r) =
  New.Inf <$> transformExpression l <*> pure o <*> transformExpression r

--------------------------------------------------
-- Matching
--------------------------------------------------

findBindings :: Old.MatchLogic -> [Symbol]
findBindings matchLogic = findBindingsAcc matchLogic []

findBindingsAcc :: Old.MatchLogic -> [Symbol] -> [Symbol]
findBindingsAcc (Old.MatchLogic contents name) xs =
  let startList =
        case name of
          Just name -> name : xs
          Nothing -> xs
      findNameSet (Old.NonPunned _ p) =
        findBindingsAcc p
      findMatchLogicSym (Old.MatchCon _name xs) acc =
        foldr findBindingsAcc acc xs
      findMatchLogicSym (Old.MatchRecord names) acc =
        foldr findNameSet acc names
      findMatchLogicSym (Old.MatchName name) acc =
        name : acc
      findMatchLogicSym (Old.MatchConst _const) acc =
        acc
   in findMatchLogicSym contents startList

transformMatch ::
  WorkingMaps m => Old.Match -> m New.Match
transformMatch (Old.Match'' on bindings) =
  New.Match'' <$> transformExpression on <*> traverse transformMatchL bindings

transformMatchL ::
  WorkingMaps m => Old.MatchL -> m New.MatchL
transformMatchL (Old.MatchL pat body) = do
  let bindings = findBindings pat
  originalBindings <- traverse saveOld bindings
  pat <- transformMatchLogic pat
  --
  traverse_ Env.addUnknown bindings
  --
  res <- New.MatchL pat <$> transformExpression body
  traverse_ restoreName originalBindings
  pure res

transformMatchLogic ::
  WorkingMaps m => Old.MatchLogic -> m New.MatchLogic
transformMatchLogic (Old.MatchLogic start name) =
  New.MatchLogic <$> (tranformMatchLogicStart start) <*> pure name

tranformMatchLogicStart ::
  WorkingMaps m => Old.MatchLogicStart -> m New.MatchLogicStart
tranformMatchLogicStart (Old.MatchCon conName logic) =
  New.MatchCon conName <$> traverse transformMatchLogic logic
tranformMatchLogicStart (Old.MatchName s) =
  pure $ New.MatchName s
tranformMatchLogicStart (Old.MatchConst c) =
  New.MatchConst <$> transformConst c
tranformMatchLogicStart (Old.MatchRecord r) =
  New.MatchRecord <$> traverse (transformNameSet transformMatchLogic) r

transformNameSet ::
  WorkingMaps m => (t -> m t1) -> Old.NameSet t -> m (New.NameSet t1)
transformNameSet p (Old.NonPunned s e) =
  New.NonPunned s <$> p e
