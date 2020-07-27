{-# LANGUAGE LiberalTypeSynonyms #-}

module Juvix.FrontendContextualise.InfixPrecedence.Transform where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import qualified Juvix.Core.Common.Context as Context
import qualified Juvix.FrontendContextualise.InfixPrecedence.Environment as Env
import qualified Juvix.FrontendContextualise.InfixPrecedence.ShuntYard as Shunt
import qualified Juvix.FrontendContextualise.InfixPrecedence.Types as New
import qualified Juvix.FrontendDesugar.RemoveDo.Types as Old
import Juvix.Library

-- Pass we care about
-- This uses the shunt algorithm
transformInfix :: Env.WorkingMaps m => Old.Infix -> m New.Expression
transformInfix inf = do
  grouped <- groupInfixs (Old.Infix inf)
  case Shunt.shunt grouped of
    Right shunted ->
      transformExpression (convertOldApplication shunted)
    Left (Shunt.Clash pred1 pred2) ->
      throw @"error" (Env.Clash pred1 pred2)
    Left Shunt.MoreEles ->
      throw @"error" Env.ImpossibleMoreEles

----------------------------------------
-- Helpers for transformInfix
----------------------------------------
precedenceConversion :: Symbol -> Context.Precedence -> Shunt.Precedence
precedenceConversion s (Context.Pred Context.Left i) =
  Shunt.Pred s Shunt.Left' i
precedenceConversion s (Context.Pred Context.Right i) =
  Shunt.Pred s Shunt.Right' i
precedenceConversion s (Context.Pred Context.NonAssoc i) =
  Shunt.Pred s Shunt.NonAssoc i

groupInfixs ::
  Env.WorkingMaps m => Old.Expression -> m (NonEmpty (Shunt.PredOrEle Old.Expression))
groupInfixs (Old.Infix (Old.Inf l s r)) = do
  let reconstructedSymbol = reconstructSymbol s
  looked <- Env.lookup reconstructedSymbol
  case looked of
    Just Context.Def {precedence} ->
      let f xs =
            precedenceConversion reconstructedSymbol precedence
              |> Shunt.Precedence
              |> flip NonEmpty.cons xs
              |> NonEmpty.cons (Shunt.Ele l)
       in fmap f (groupInfixs r)
    _ -> throw @"error" (Env.UnknownSymbol reconstructedSymbol)
groupInfixs e = pure (Shunt.Ele e :| [])

convertOldApplication :: Shunt.Application Old.Expression -> Old.Expression
convertOldApplication (Shunt.Single e) =
  e
convertOldApplication (Shunt.App s app1 app2) =
  fmap convertOldApplication (app1 :| [app2])
    |> Old.App (Old.Name (deconstructSymbol s))
    |> Old.Application

-- TODO ∷ bad hack I'll have to change
reconstructSymbol :: NonEmpty Symbol -> Symbol
reconstructSymbol =
  intern . foldr (\x acc -> unintern x <> "." <> acc) mempty

deconstructSymbol :: Symbol -> NonEmpty Symbol
deconstructSymbol =
  NonEmpty.fromList . fmap internText . Text.split (== '.') . textify

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

transformTopLevel ::
  Env.WorkingMaps m => Old.TopLevel -> m New.TopLevel
transformTopLevel (Old.Type t) = New.Type <$> transformType t
transformTopLevel (Old.ModuleOpen t) = New.ModuleOpen <$> transformModuleOpen t
transformTopLevel (Old.Function t) = New.Function <$> transformFunction t
transformTopLevel Old.TypeClass = pure New.TypeClass
transformTopLevel Old.TypeClassInstance = pure New.TypeClassInstance

transformExpression ::
  Env.WorkingMaps m => Old.Expression -> m New.Expression
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
transformExpression (Old.Infix i) = transformInfix i
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

transformFunctionLike ::
  Env.WorkingMaps m => Old.FunctionLike Old.Expression -> m (New.FunctionLike New.Expression)
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
  Env.WorkingMaps m => Old.ModuleOpen -> m New.ModuleOpen
transformModuleOpen (Old.Open mod) = do
  modify @"new" (Context.open (reconstructSymbol mod))
  pure $ New.Open mod

transformModuleOpenExpr ::
  Env.WorkingMaps m => Old.ModuleOpenExpr -> m New.ModuleOpenExpr
transformModuleOpenExpr (Old.OpenExpress modName expr) = do
  looked <- Env.lookup (reconstructSymbol modName)
  case looked of
    Just Context.Def {} -> res
    Just Context.TypeDeclar {} -> res
    Just Context.Unknown {} -> res
    Nothing -> res
    Just (Context.Record innerC _mTy) -> do
      let newSymb = fmap fst $ Context.toList innerC
      savedDef <- traverse saveOld newSymb
      --
      modify @"new" (Context.open (reconstructSymbol modName))
      res <- res
      --
      _ <- traverse restoreName savedDef
      pure res
  where
    res = New.OpenExpress modName <$> transformExpression expr

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
  Env.WorkingMaps m => Old.Lambda -> m New.Lambda
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

transformLet :: Env.WorkingMaps m => Old.Let -> m New.Let
transformLet (Old.LetGroup name bindings body) = do
  originalVal <- Env.lookup name -- look up in "new" state
  let transform = do
        Env.addUnknown name
        transformedBindings <- traverse transformFunctionLike bindings
        let def = decideRecordOrDef transformedBindings Nothing
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
  Env.WorkingMaps m => Old.LetType -> m New.LetType
transformLetType (Old.LetType'' typ expr) = do
  let typeName = Old.typeName' typ
  originalVal <- Env.lookup typeName
  let transform = do
        Env.addUnknown typeName
        transformedType <- transformType typ
        let def = Context.TypeDeclar transformedType
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

transformMatchL ::
  Env.WorkingMaps m => Old.MatchL -> m New.MatchL
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
  Env.WorkingMaps m => Old.MatchLogic -> m New.MatchLogic
transformMatchLogic (Old.MatchLogic start name) =
  New.MatchLogic <$> (tranformMatchLogicStart start) <*> pure name

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
  New.NonPunned s <$> p e
