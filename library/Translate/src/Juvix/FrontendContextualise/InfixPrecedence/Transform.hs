{-# LANGUAGE LiberalTypeSynonyms #-}

module Juvix.FrontendContextualise.InfixPrecedence.Transform where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Juvix.Core.Common.Context as Context
import qualified Juvix.Core.Common.NameSpace as NameSpace
import qualified Juvix.FrontendContextualise.InfixPrecedence.Environment as Env
import qualified Juvix.FrontendContextualise.InfixPrecedence.ShuntYard as Shunt
import qualified Juvix.FrontendContextualise.InfixPrecedence.Types as New
import qualified Juvix.FrontendContextualise.ModuleOpen.Types as Old
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol
import Prelude (error)

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
precedenceConversion ::
  NameSymbol.T -> Context.Precedence -> Shunt.Precedence NameSymbol.T
precedenceConversion s (Context.Pred Context.Left i) =
  Shunt.Pred s Shunt.Left' i
precedenceConversion s (Context.Pred Context.Right i) =
  Shunt.Pred s Shunt.Right' i
precedenceConversion s (Context.Pred Context.NonAssoc i) =
  Shunt.Pred s Shunt.NonAssoc i

groupInfixs ::
  Env.WorkingMaps m =>
  Old.Expression ->
  m (NonEmpty (Shunt.PredOrEle NameSymbol.T Old.Expression))
groupInfixs (Old.Infix (Old.Inf l s r)) = do
  looked <- Env.lookup s
  let f precedence xs =
        precedenceConversion s precedence
          |> Shunt.Precedence
          |> flip NonEmpty.cons xs
          -- we cons l and not r, as "3 + 4 + 5 * 6"
          -- parses as "3 + (4 + (5 * 6))"
          -- thus the left is always an element
          |> NonEmpty.cons (Shunt.Ele l)
      --
      continuePref ::
        Env.WorkingMaps m =>
        Maybe (Context.Definition term ty sumRep) ->
        m (NonEmpty (Shunt.PredOrEle NameSymbol.T Old.Expression)) ->
        m (NonEmpty (Shunt.PredOrEle NameSymbol.T Old.Expression))
      continuePref (Just Context.Def {precedence}) _maybeF =
        fmap (f precedence) (groupInfixs r)
      continuePref (Just (Context.Information is)) _maybeF =
        case Context.precedenceOf is of
          Nothing -> throw @"error" (Env.UnknownSymbol s)
          Just pr -> fmap (f pr) (groupInfixs r)
      -- This seems incorrect, see the case were any shadowing may occur.
      -- (Top level vs local or private vs public)
      continuePref (Just _) _maybeF =
        throw @"error" (Env.UnknownSymbol s)
      continuePref Nothing maybeF =
        maybeF
  continuePref (Context.extractValue <$> looked) $ do
    oldLook <- Env.ask s
    -- not in the new map, checkout the old
    continuePref (Context.extractValue <$> oldLook) $
      throw @"error" (Env.UnknownSymbol s)
groupInfixs e = pure (Shunt.Ele e :| [])

convertOldApplication ::
  Shunt.Application NameSymbol.T Old.Expression -> Old.Expression
convertOldApplication (Shunt.Single e) =
  e
convertOldApplication (Shunt.App s app1 app2) =
  fmap convertOldApplication (app1 :| [app2])
    |> Old.App (Old.Name s)
    |> Old.Application

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
              NameSpace.insert
                (NameSpace.Pub (NonEmpty.head s))
                (decideRecordOrDef (New.Like [] e :| []) Nothing)
         in Context.Record (foldr f NameSpace.empty i) ty
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

transformDef ::
  Env.WorkingMaps m =>
  Env.Old Context.Definition ->
  NameSpace.From Symbol ->
  m (Env.New Context.Definition)
transformDef (Context.Def usage mTy term prec) _ =
  Context.Def usage
    <$> traverse transformSignature mTy
    <*> traverse transformFunctionLike term
    <*> pure prec
--
transformDef (Context.TypeDeclar repr) _ =
  Context.TypeDeclar <$> transformType repr
--
transformDef (Context.Unknown mTy) _ =
  Context.Unknown <$> traverse transformSignature mTy
--
transformDef Context.CurrentNameSpace _ =
  pure Context.CurrentNameSpace
--
transformDef (Context.Information is) _ =
  pure (Context.Information is)
--
transformDef (Context.Record _contents mTy) name' = do
  sig <- traverse transformSignature mTy
  old <- get @"old"
  let name = NameSpace.extractValue name'
      newMod = pure Context.topLevelName <> Context.currentName old <> pure name
  -- switch to the new namespace
  updateSym newMod
  -- do our transform
  transformInner
  -- transform back
  updateSym (pure Context.topLevelName <> Context.currentName old)
  -- sadly this currently only adds it to the public, so we have to remove it
  looked <- fmap NameSpace.extractValue <$> Env.lookupCurrent name
  Env.remove name'
  case looked of
    Just (Context.Record record _) ->
      pure (Context.Record record sig)
    Nothing -> error "Does not happen: record lookup is nothing"
    Just __ -> error "Does not happen: record lookup is Just not a record!"

-- we work on the topMap
transformC :: Env.WorkingMaps m => m ()
transformC = do
  old <- get @"old"
  let oldC = Context.topList old
      --
      updateSym' sym = updateSym (Context.topLevelName :| [sym])
  case oldC of
    [(sym, _)] -> do
      updateSym' sym
      transformInner
    (sym, _) : (sym2, _) : _ -> do
      updateSym' sym
      transformInner
      -- this way we update our namespace before
      -- removing from top
      -- likely the next way around we work on sym2
      updateSym' sym2
      modify @"old" (Context.removeTop sym)
      transformC
    [] -> pure ()

transformInner ::
  Env.WorkingMaps m => m ()
transformInner = do
  old <- get @"old"
  let oldC = Context.toList old
  case oldC of
    NameSpace.List {publicL = [], privateL = []} ->
      pure ()
    NameSpace.List {publicL = ((sym, def) : _), privateL = _} -> do
      newDef <- transformDef def (NameSpace.Pub sym)
      Env.add (NameSpace.Pub sym) newDef
      Env.removeOld (NameSpace.Pub sym)
      transformInner
    NameSpace.List {publicL = [], privateL = ((sym, def) : _)} -> do
      newDef <- transformDef def (NameSpace.Priv sym)
      Env.add (NameSpace.Priv sym) newDef
      Env.removeOld (NameSpace.Priv sym)
      transformInner

transformExpression ::
  Env.WorkingMaps m => Old.Expression -> m New.Expression
transformExpression (Old.Tuple t) = New.Tuple <$> transformTuple t
transformExpression (Old.List t) = New.List <$> transformList t
transformExpression (Old.Primitive t) = New.Primitive <$> transformPrim t
transformExpression (Old.Constant c) = New.Constant <$> transformConst c
transformExpression (Old.Let l) = New.Let <$> transformLet l
transformExpression (Old.LetType l) = New.LetType <$> transformLetType l
transformExpression (Old.Match m) = New.Match <$> transformMatch m
transformExpression (Old.Name n) = pure $ New.Name n
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
transformExpression (Old.Parened e) =
  New.Parened <$> transformExpression e
transformExpression (Old.DeclarationE e) =
  New.DeclarationE <$> transformDeclarationExpression e

--------------------------------------------------------------------------------
-- Declaration
--------------------------------------------------------------------------------

transformDeclarationExpression ::
  Env.WorkingMaps m => Old.DeclarationExpression -> m New.DeclarationExpression
transformDeclarationExpression (Old.DeclareExpession i e) =
  New.DeclareExpession <$> transformDeclaration i <*> transformExpression e

transformDeclaration :: Env.WorkingMaps m => Old.Declaration -> m New.Declaration
transformDeclaration (Old.Infixivity i) =
  New.Infixivity <$> transformInfixDeclar i

-- TODO ∷ update map to reflect the infixivity changes!!!!!!

transformInfixDeclar :: Env.WorkingMaps m => Old.InfixDeclar -> m New.InfixDeclar
transformInfixDeclar (Old.AssocL n i) = pure (New.AssocL n i)
transformInfixDeclar (Old.AssocR n i) = pure (New.AssocR n i)
transformInfixDeclar (Old.NonAssoc n i) = pure (New.NonAssoc n i)

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
  Env.WorkingMaps m =>
  Old.FunctionLike Old.Expression ->
  m (New.FunctionLike New.Expression)
transformFunctionLike (Old.Like args body) =
  protectSymbols (accBindings args) $
    New.Like <$> traverse transformArg args <*> transformExpression body

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

transformLambda ::
  Env.WorkingMaps m => Old.Lambda -> m New.Lambda
transformLambda (Old.Lamb args body) =
  protectSymbols bindings $
    New.Lamb <$> traverse transformMatchLogic args <*> transformExpression body
  where
    bindings = foldr (\x acc -> findBindings x <> acc) [] args

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
  protectSymbols [name] $ do
    transformedBindings <- traverse transformFunctionLike bindings
    --
    Env.add (NameSpace.Priv name) (decideRecordOrDef transformedBindings Nothing)
    --
    res <- New.LetGroup name transformedBindings <$> transformExpression body
    -- don't know where we came from!
    Env.remove (NameSpace.Priv name)
    pure res

transformLetType ::
  Env.WorkingMaps m => Old.LetType -> m New.LetType
transformLetType (Old.LetType'' typ expr) = do
  let typeName = Old.typeName' typ
  protectSymbols [typeName] $ do
    transformedType <- transformType typ
    --
    Env.add (NameSpace.Priv typeName) (Context.TypeDeclar transformedType)
    --
    res <- New.LetType'' transformedType <$> transformExpression expr
    -- don't know where we came from!
    Env.remove (NameSpace.Priv typeName)
    pure res

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
transformMatchL (Old.MatchL pat body) =
  protectSymbols
    (findBindings pat)
    (New.MatchL <$> transformMatchLogic pat <*> transformExpression body)

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
  New.NonPunned s <$> p e

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------
protectSymbols :: Env.WorkingMaps m => [Symbol] -> m a -> m a
protectSymbols syms op = do
  originalBindings <- traverse saveOld syms
  traverse_ Env.addUnknownGlobal (fmap snd originalBindings)
  res <- op
  traverse_ restoreName originalBindings
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

updateSym :: Env.WorkingMaps m => Context.NameSymbol -> m ()
updateSym sym = do
  old <- get @"old"
  new <- get @"new"
  case Context.switchNameSpace sym old of
    -- bad Error for now
    Left ____ -> throw @"error" (Env.PathError sym)
    Right map -> put @"old" map
  -- have to do this again sadly
  case Context.switchNameSpace sym new of
    Left ____ -> throw @"error" (Env.PathError sym)
    Right map -> put @"new" map
