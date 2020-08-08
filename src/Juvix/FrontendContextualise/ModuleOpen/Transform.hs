{-# LANGUAGE LiberalTypeSynonyms #-}

module Juvix.FrontendContextualise.ModuleOpen.Transform where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Juvix.Core.Common.Context as Context
import qualified Juvix.Core.Common.NameSpace as NameSpace
import qualified Juvix.FrontendContextualise.ModuleOpen.Environment as Env
import qualified Juvix.FrontendContextualise.ModuleOpen.Types as New
import qualified Juvix.FrontendDesugar.RemoveDo.Types as Old
import Juvix.Library

-- Sadly for this pass we have to change every symbol affecting function

-- we also have transformSymbol wherever nonemtpy symbol is used
-- this makes sures we correctly qualify a module

transformSymbol ::
  HasState "modMap" Env.ModuleMap m => NonEmpty Symbol -> m (NonEmpty Symbol)
transformSymbol = Env.qualifyName

transformModuleOpenExpr ::
  Env.WorkingMaps m => Old.ModuleOpenExpr -> m New.Expression
transformModuleOpenExpr (Old.OpenExpress modName expr) = do
  fullQualified <- Env.qualifyName modName
  let err = throw @"error" (Env.UnknownModule reconstructed)
  looked <- Env.lookup fullQualified
  case looked of
    Just Context.Def {} -> err
    Just Context.TypeDeclar {} -> err
    Just Context.Unknown {} -> err
    Nothing -> err
    Just (Context.Record innerC _mTy) -> do
      -- Fine to just have the public names
      -- we are only tracking what this can use, not doing
      -- replacements
      let newSymb = fst <$> Context.toList innerC
      savedDef <- traverse saveOldOpen newSymb
      --
      traverse_ (`Env.addModMap` fullQualified) newSymb
      res <- transformExpression expr
      --
      traverse_ restoreNameOpen savedDef
      pure res

saveOldOpen ::
  HasState "modMap" Env.ModuleMap m => Symbol -> m (Maybe (NonEmpty Symbol), Symbol)
saveOldOpen sym =
  (,sym) <$> Env.lookupModMap sym

restoreNameOpen ::
  HasState "modMap" Env.ModuleMap m => (Maybe (NonEmpty Symbol), Symbol) -> m ()
restoreNameOpen (Just def, sym) = Env.addModMap sym def
restoreNameOpen (Nothing, sym) = Env.removeModMap sym

--------------------------------------------------
-- Symbol Binding
--------------------------------------------------

transformLet :: Env.WorkingMaps m => Old.Let -> m New.Let
transformLet (Old.LetGroup name bindings body) = do
  originalVal <- Env.lookup name
  qualifiedName <- Env.lookupModMap name
  let transform = do
        Env.addUnknown name
        Env.removeModMap name
        transformedBindings <- traverse transformFunctionLike bindings
        let def = decideRecordOrDef transformedBindings Nothing
        Env.add name def -- add to new context
        New.LetGroup name transformedBindings <$> transformExpression body
  res <- transform
  restoreName (originalVal, name)
  restoreNameOpen (qualifiedName, name)
  pure res

transformLetType ::
  Env.WorkingMaps m => Old.LetType -> m New.LetType
transformLetType (Old.LetType'' typ expr) = do
  let typeName = Old.typeName' typ
  originalVal <- Env.lookup typeName
  qualifiedName <- Env.lookupModMap typeName
  let transform = do
        Env.addUnknown typeName
        Env.removeModMap typeName
        transformedType <- transformType typ
        let def = Context.TypeDeclar transformedType
        Env.add typeName def -- add to new context
        New.LetType'' transformedType <$> transformExpression expr
  res <- transform
  restoreName (originalVal, typeName)
  restoreNameOpen (qualifiedName, typeName)
  pure res

transformFunctionLike ::
  Env.WorkingMaps m => Old.FunctionLike Old.Expression -> m (New.FunctionLike New.Expression)
transformFunctionLike (Old.Like args body) = do
  let bindings = accBindings args
  originalBindings <- traverse saveOld bindings
  originalQualified <- traverse saveOldOpen bindings
  transArgs <- traverse transformArg args
  --
  traverse_ Env.addUnknown bindings
  traverse_ Env.removeModMap bindings
  --
  res <- New.Like transArgs <$> transformExpression body
  --
  traverse_ restoreName originalBindings
  traverse_ restoreNameOpen originalQualified
  pure res

transformMatchL ::
  Env.WorkingMaps m => Old.MatchL -> m New.MatchL
transformMatchL (Old.MatchL pat body) = do
  let bindings = findBindings pat
  originalBindings <- traverse saveOld bindings
  originalQualified <- traverse saveOldOpen bindings
  pat <- transformMatchLogic pat
  --
  traverse_ Env.addUnknown bindings
  traverse_ Env.removeModMap bindings
  --
  res <- New.MatchL pat <$> transformExpression body
  traverse_ restoreName originalBindings
  traverse_ restoreNameOpen originalQualified
  pure res

transformLambda ::
  Env.WorkingMaps m => Old.Lambda -> m New.Lambda
transformLambda (Old.Lamb args body) = do
  let bindings = findBindings (NonEmpty.head args)
  originalBindings <- traverse saveOld bindings
  originalQualified <- traverse saveOldOpen bindings
  transArgs <- transformMatchLogic (NonEmpty.head args)
  --
  traverse_ Env.addUnknown bindings
  traverse_ Env.removeModMap bindings
  --
  res <- New.Lamb (pure transArgs) <$> transformExpression body
  traverse_ restoreName originalBindings
  traverse_ restoreNameOpen originalQualified
  pure res

--------------------------------------------------------------------------------
-- Record/Def Decision function... update when contextify version updates
--------------------------------------------------------------------------------

-- | decideRecordOrDef tries to figure out
-- if a given defintiion is a record or a definition
decideRecordOrDef ::
  NonEmpty (New.FunctionLike New.Expression) ->
  Maybe New.Signature ->
  Env.New Context.Definition
decideRecordOrDef xs ty
  | len == 1 && emptyArgs args =
    -- For the two matched cases eventually
    -- turn these into record expressions
    case body of
      New.ExpRecord (New.ExpressionRecord i) ->
        -- the type here can eventually give us arguments though looking at the
        -- lambda for e, and our type can be found out similarly by looking at types
        let f (New.NonPunned s e) =
              Context.add
                (NonEmpty.head s)
                (decideRecordOrDef (New.Like [] e :| []) Nothing)
         in Context.Record (foldr f Context.empty i) ty
      New.Let _l ->
        def
      _ -> def
  | otherwise = def
  where
    len = length xs
    New.Like args body = NonEmpty.head xs
    def = Context.Def Nothing ty xs Context.default'

emptyArgs :: [a] -> Bool
emptyArgs [] = True
emptyArgs (_ : _) = False

--------------------------------------------------------------------------------
-- Boilerplate Transforms
--------------------------------------------------------------------------------

transformContext :: Env.Old Context.T -> Either Env.Error (Env.New Context.T)
transformContext ctx =
  case Env.runEnv transformC ctx of
    (Right _, env) -> Right (Env.new env)
    (Left e, _) -> Left e

transformContextInner :: Env.WorkingMaps m => Env.Old Context.T -> m (Env.New Context.T)
transformContextInner ctx =
  case Env.runEnv transformC ctx of
    (Right _, env) -> pure (Env.new env)
    (Left e, _) -> throw @"error" e

transformDef ::
  Env.WorkingMaps m => Env.Old Context.Definition -> m (Env.New Context.Definition)
transformDef (Context.Def usage mTy term prec) =
  Context.Def usage
    <$> traverse transformSignature mTy
    <*> traverse transformFunctionLike term
    <*> pure prec
transformDef (Context.Record contents mTy) =
  Context.Record <$> transformContextInner contents <*> traverse transformSignature mTy
transformDef (Context.TypeDeclar repr) =
  Context.TypeDeclar <$> transformType repr
transformDef (Context.Unknown mTy) =
  Context.Unknown <$> traverse transformSignature mTy

transformC ::
  Env.WorkingMaps m => m ()
transformC = do
  old <- get @"old"
  let oldC = Context.toList old
  case oldC of
    (sym, def) : _ -> do
      newDef <- transformDef def
      Env.add sym newDef
      Env.removeOld sym
      transformC
    [] -> pure ()

transformExpression ::
  Env.WorkingMaps m => Old.Expression -> m New.Expression
transformExpression (Old.Tuple t) = New.Tuple <$> transformTuple t
transformExpression (Old.List t) = New.List <$> transformList t
transformExpression (Old.Primitive t) = New.Primitive <$> transformPrim t
transformExpression (Old.Constant c) = New.Constant <$> transformConst c
transformExpression (Old.Let l) = New.Let <$> transformLet l
transformExpression (Old.LetType l) = New.LetType <$> transformLetType l
transformExpression (Old.Match m) = New.Match <$> transformMatch m
transformExpression (Old.Infix i) = New.Infix <$> transformInfix i
transformExpression (Old.Name n) =
  New.Name <$> transformSymbol n
transformExpression (Old.OpenExpr n) =
  transformModuleOpenExpr n
transformExpression (Old.Lambda l) = New.Lambda <$> transformLambda l
transformExpression (Old.Application a) =
  New.Application <$> transformApplication a
transformExpression (Old.Block b) = New.Block <$> transformBlock b
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
  Env.WorkingMaps m => Old.Type -> m New.Type
transformType (Old.Typ usage name' args form) =
  New.Typ
    <$> traverse transformExpression usage
    <*> pure name'
    <*> pure args
    <*> transformData form

--------------------------------------------------
-- Arrows
--------------------------------------------------

transformNamedType ::
  Env.WorkingMaps m => Old.NamedType -> m New.NamedType
transformNamedType (Old.NamedType' name exp) =
  New.NamedType' <$> transformName name <*> transformExpression exp

transformTypeRefine ::
  Env.WorkingMaps m => Old.TypeRefine -> m New.TypeRefine
transformTypeRefine (Old.TypeRefine name refine) =
  New.TypeRefine <$> transformExpression name <*> transformExpression refine

--------------------------------------------------
-- Types Misc
--------------------------------------------------

transformName ::
  Env.WorkingMaps m => Old.Name -> m New.Name
transformName (Old.Implicit s) = pure $ New.Implicit s
transformName (Old.Concrete s) = pure $ New.Concrete s

transformArrowSymbol ::
  Env.WorkingMaps m => Old.ArrowSymbol -> m New.ArrowSymbol
transformArrowSymbol (Old.ArrowUse usage) =
  pure $ New.ArrowUse usage
transformArrowSymbol (Old.ArrowExp e) =
  New.ArrowExp <$> transformExpression e

transformUniverseExpression ::
  Env.WorkingMaps m => Old.UniverseExpression -> m New.UniverseExpression
transformUniverseExpression (Old.UniverseExpression s) =
  pure $ New.UniverseExpression s

--------------------------------------------------
-- ADTs
--------------------------------------------------

transformData ::
  Env.WorkingMaps m => Old.Data -> m New.Data
transformData (Old.Arrowed exp adt) =
  New.Arrowed <$> transformExpression exp <*> transformAdt adt
transformData (Old.NonArrowed adt) =
  New.NonArrowed <$> transformAdt adt

transformAdt ::
  Env.WorkingMaps m => Old.Adt -> m New.Adt
transformAdt (Old.Sum oldsu) = New.Sum <$> traverse transformSum oldsu
transformAdt (Old.Product p) = New.Product <$> transformProduct p

transformSum ::
  Env.WorkingMaps m => Old.Sum -> m New.Sum
transformSum (Old.S sym prod) =
  New.S sym <$> traverse transformProduct prod

transformProduct ::
  Env.WorkingMaps m => Old.Product -> m New.Product
transformProduct (Old.Record rec') = New.Record <$> transformRecord rec'
transformProduct (Old.Arrow arrow) = New.Arrow <$> transformExpression arrow
transformProduct (Old.ADTLike adt) = New.ADTLike <$> traverse transformExpression adt

transformRecord ::
  Env.WorkingMaps m => Old.Record -> m New.Record
transformRecord (Old.Record'' fields sig) =
  New.Record'' <$> traverse transformNameType fields <*> traverse transformExpression sig

transformNameType ::
  Env.WorkingMaps m => Old.NameType -> m New.NameType
transformNameType (Old.NameType' sig name) =
  New.NameType' <$> transformExpression sig <*> transformName name

transformFunction ::
  Env.WorkingMaps m => Old.Function -> m New.Function
transformFunction (Old.Func name f sig) =
  New.Func name <$> traverse transformFunctionLike f <*> traverse transformSignature sig

argLogic :: Old.Arg -> Old.MatchLogic
argLogic (Old.ConcreteA x) = x
argLogic (Old.ImplicitA x) = x

accBindings :: [Old.Arg] -> [Symbol]
accBindings = concatMap (findBindings . argLogic)

transformArg ::
  Env.WorkingMaps m => Old.Arg -> m New.Arg
transformArg (Old.ConcreteA ml) = New.ConcreteA <$> transformMatchLogic ml
transformArg (Old.ImplicitA ml) = New.ImplicitA <$> transformMatchLogic ml

--------------------------------------------------------------------------------
-- Signatures
--------------------------------------------------------------------------------

transformSignature ::
  Env.WorkingMaps m => Old.Signature -> m New.Signature
transformSignature (Old.Sig name usage arrow constraints) =
  New.Sig name
    <$> traverse transformExpression usage
    <*> transformExpression arrow
    <*> traverse transformExpression constraints

--------------------------------------------------------------------------------
-- Expression
--------------------------------------------------------------------------------

transformArrowExp ::
  Env.WorkingMaps m => Old.ArrowExp -> m New.ArrowExp
transformArrowExp (Old.Arr' left usage right) =
  New.Arr'
    <$> transformExpression left
    <*> transformExpression usage
    <*> transformExpression right

transformPrim :: Env.WorkingMaps m => Old.Primitive -> m New.Primitive
transformPrim (Old.Prim p) =
  pure (New.Prim p)

transformTuple :: Env.WorkingMaps m => Old.Tuple -> m New.Tuple
transformTuple (Old.TupleLit t) =
  New.TupleLit <$> traverse transformExpression t

transformList :: Env.WorkingMaps m => Old.List -> m New.List
transformList (Old.ListLit t) =
  New.ListLit <$> traverse transformExpression t

transformConst ::
  Env.WorkingMaps m => Old.Constant -> m New.Constant
transformConst (Old.Number numb) = New.Number <$> transformNumb numb
transformConst (Old.String str) = New.String <$> transformString str

transformNumb ::
  Env.WorkingMaps m => Old.Numb -> m New.Numb
transformNumb (Old.Integer' i) = pure $ New.Integer' i
transformNumb (Old.Double' d) = pure $ New.Double' d

transformString ::
  Env.WorkingMaps m => Old.String' -> m New.String'
transformString (Old.Sho t) = pure $ New.Sho t

transformBlock ::
  Env.WorkingMaps m => Old.Block -> m New.Block
transformBlock (Old.Bloc expr) = New.Bloc <$> transformExpression expr

protectSymbols syms op = do
  originalBindings <- traverse saveOld syms
  traverse_ Env.addUnknownGlobal (fmap snd originalBindings)
  res <- op
  traverse restoreName originalBindings
  pure res

saveOld ::
  HasState "new" (Context.T term ty sumRep) f =>
  Symbol ->
  f (Maybe (Context.Definition term ty sumRep), Context.From Symbol)
saveOld sym = do
  looked <- Env.lookup sym
  case looked of
    Nothing ->
      pure (Nothing, Context.Current (NameSpace.Pub sym))
    Just (Context.Outside def) ->
      pure (Just def, Context.Outside sym)
    Just (Context.Current (NameSpace.Pub def)) ->
      pure (Just def, Context.Current (NameSpace.Pub sym))
    Just (Context.Current (NameSpace.Priv def)) ->
      pure (Just def, Context.Current (NameSpace.Priv sym))

restoreName ::
  HasState "new" (Context.T term ty sumRep) m =>
  (Maybe (Context.Definition term ty sumRep), Context.From Symbol) ->
  m ()
restoreName (def, Context.Current sym) =
  case def of
    Just def ->
      Env.add sym def
    Nothing ->
      Env.remove sym
restoreName (def, Context.Outside sym) =
  -- we have to turn a symbol into a NameSymbol.T
  -- we just have to pure it, the symbol we get
  -- should not have any .'s inside of it
  case def of
    Just def ->
      Env.addGlobal (sym :| []) def
    Nothing ->
      Env.removeGlobal (sym :| [])

transformApplication ::
  Env.WorkingMaps m => Old.Application -> m New.Application
transformApplication (Old.App fun args) =
  New.App <$> transformExpression fun <*> traverse transformExpression args

transformExpRecord ::
  Env.WorkingMaps m => Old.ExpRecord -> m New.ExpRecord
transformExpRecord (Old.ExpressionRecord fields) =
  New.ExpressionRecord <$> traverse (transformNameSet transformExpression) fields

--------------------------------------------------
-- Symbol Binding
--------------------------------------------------

transformInfix ::
  Env.WorkingMaps m => Old.Infix -> m New.Infix
transformInfix (Old.Inf l o r) =
  New.Inf <$> transformExpression l <*> transformSymbol o <*> transformExpression r

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
  Env.WorkingMaps m => Old.Match -> m New.Match
transformMatch (Old.Match'' on bindings) =
  New.Match'' <$> transformExpression on <*> traverse transformMatchL bindings

transformMatchLogic ::
  Env.WorkingMaps m => Old.MatchLogic -> m New.MatchLogic
transformMatchLogic (Old.MatchLogic start name) =
  New.MatchLogic <$> tranformMatchLogicStart start <*> pure name

tranformMatchLogicStart ::
  Env.WorkingMaps m => Old.MatchLogicStart -> m New.MatchLogicStart
tranformMatchLogicStart (Old.MatchCon conName logic) =
  New.MatchCon conName <$> traverse transformMatchLogic logic
tranformMatchLogicStart (Old.MatchName s) =
  pure $ New.MatchName s
tranformMatchLogicStart (Old.MatchConst c) =
  New.MatchConst <$> transformConst c
tranformMatchLogicStart (Old.MatchRecord r) =
  New.MatchRecord <$> traverse (transformNameSet transformMatchLogic) r

transformNameSet ::
  Env.WorkingMaps m => (t -> m t1) -> Old.NameSet t -> m (New.NameSet t1)
transformNameSet p (Old.NonPunned s e) =
  New.NonPunned <$> transformSymbol s <*> p e
