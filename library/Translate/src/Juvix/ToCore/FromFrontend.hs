{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}

module Juvix.ToCore.FromFrontend
  ( module Juvix.ToCore.FromFrontend,
    module Juvix.ToCore.Types,
  )
where

import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty as NonEmpty
import qualified Generics.SYB as SYB
import qualified Juvix.Core.Common.Context as Ctx
import qualified Juvix.Core.HR as HR
import qualified Juvix.Core.IR as IR
import qualified Juvix.Core.Parameterisation as P
import Juvix.Core.Translate (hrToIR)
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.Sexp as Sexp
import qualified Juvix.Library.Usage as Usage
import Juvix.ToCore.Types
import Prelude (error)

type ReduceEff primTy primVal m =
  ( HasThrowFF primTy primVal m,
    HasParam primTy primVal m,
    HasCoreSigs primTy primVal m
  )

paramConstant' ::
  P.Parameterisation primTy primVal ->
  Sexp.Atom ->
  Maybe primVal
paramConstant' p Sexp.N {atomNum} = P.intVal p atomNum
paramConstant' _p Sexp.A {} = Nothing

transformTermIR ::
  ( 
    HasPatVars m,
    HasThrowFF primTy primVal m,
    HasParam primTy primVal m,
    HasCoreSigs primTy primVal m
  ) =>
  NameSymbol.Mod ->
  Sexp.T ->
  m (IR.Term primTy primVal)
transformTermIR q fe = do
  hrToIR <$> transformTermHR q fe
  -- TODO: Bring what everywhereM does back without Data.Data
  -- SYB.everywhereM (SYB.mkM transformPatVar) . hrToIR =<< transformTermHR q fe

transformPatVar :: HasPatVars m => IR.Name -> m IR.Name
transformPatVar (IR.Global name) =
  gets @"patVars" $
    maybe (IR.Global name) IR.Pattern
      . HM.lookup name
transformPatVar p = pure p

paramConstant ::
  (HasParam primTy primVal m, HasThrowFF primTy primVal m) =>
  Sexp.Atom ->
  m primVal
paramConstant k = do
  p <- ask @"param"
  case paramConstant' p k of
    Just x -> pure x
    Nothing -> throwFF $ UnsupportedConstant (Sexp.Atom k)

-- | N.B. doesn't deal with pattern variables since HR doesn't have them.
-- 'transformTermIR' does that.
transformTermHR ::
  ReduceEff primTy primVal m => NameSymbol.Mod -> Sexp.T -> m (HR.Term primTy primVal)
transformTermHR _ (Sexp.Atom a@Sexp.N {}) =
  HR.Prim <$> paramConstant a
transformTermHR q (Sexp.Atom Sexp.A {atomName}) =
  toName <$> lookupSig' (Just q) atomName
  where
    toName = HR.Elim . HR.Var . maybe atomName fst
transformTermHR q p@(name Sexp.:> form)
  -- Unimplemented cases
  -- 1. _refinement_
  --    - TODO :: the name will only become relevant (outside of arrows)
  --              when refinements are supported
  -- 2. _universe names_
  --    - TODO :: for universe polymorphism
  -- 3. _declaim_
  --    - TODO :: We ignore the declration currently
  | named ":record-no-pun" = throwFF $ RecordUnimplemented p
  | named ":refinement" = throwFF $ RefinementsUnimplemented p
  | named ":let-type" = throwFF $ ExprUnimplemented p
  | named ":list" = throwFF $ ListUnimplemented p
  | named "case" = throwFF $ ExprUnimplemented p
  | named ":u" = throwFF $ UniversesUnimplemented p
  -- Rest
  | named ":custom-arrow" = transformArrow q form
  | named ":let-match" = transformSimpleLet q form
  | named ":primitive" = transPrim form
  | named ":declaim" = transformTermHR q (Sexp.cadr form) -- skip declar
  | named ":lambda" = transformSimpleLambda q form
  | named ":progn" = transformTermHR q (Sexp.car form)
  | named ":paren" = transformTermHR q (Sexp.car form)
  | named ":tuple",
    Just xs <- Sexp.toList form =
    makeTuple <$> traverse (transformTermHR q) xs
  | otherwise =
    transformApplication q p
  where
    named = Sexp.isAtomNamed name
transformTermHR _ Sexp.Nil = error "malfromed"

transformApplication ::
  ReduceEff primTy primVal m => NameSymbol.Mod -> Sexp.T -> m (HR.Term primTy primVal)
transformApplication q (f Sexp.:> args)
  | Just xs <- Sexp.toList args =
    getSpecialE q f >>= flip go xs
  where
    go Nothing xs = do
      f' <- toElim f =<< transformTermHR q f
      HR.Elim . foldl HR.App f' <$> traverse (transformTermHR q) xs
    go (Just s) xs = case s of
      ArrowS Nothing -> do
        ~[π, a, b] <- nargs s 3 xs
        π <- transformUsage q π
        go (Just (ArrowS (Just π))) [a, b]
      ArrowS (Just π) -> do
        ~[xa, b] <- nargs s 2 xs
        (x, a) <- namedArg q xa
        HR.Pi π x a <$> transformTermHR q b
      PairS Nothing -> do
        ~[π, a, b] <- nargs s 3 xs
        π <- transformUsage q π
        go (Just (PairS (Just π))) [a, b]
      PairS (Just π) -> do
        ~[xa, b] <- nargs s 2 xs
        (x, a) <- namedArg q xa
        HR.Sig π x a <$> transformTermHR q b
      ColonS -> do
        ~[a, b] <- nargs s 2 xs
        a <- transformTermHR q a
        b <- transformTermHR q b
        -- FIXME metavars for usage & universe
        pure $ HR.Elim $ HR.Ann (Usage.SNat 1) a b 0
      TypeS -> do
        ~[i] <- nargs s 1 xs
        HR.Star <$> transformUniverse i
      OmegaS ->
        throwFF UnexpectedOmega
    nargs s n xs
      | length xs == n = pure xs
      | otherwise = throwFF $ WrongNumberBuiltinArgs s n args
transformApplication _ _ = error "malformed application"

namedArg ::
  ReduceEff primTy primVal m =>
  NameSymbol.Mod ->
  Sexp.T ->
  m (NameSymbol.T, HR.Term primTy primVal)
namedArg q e = transformTermHR q e >>= \case
  NamedArgTerm x ty -> pure (NameSymbol.fromSymbol x, ty)
  ty -> pure ("" :| [], ty)

makeTuple :: [HR.Term primTy primVal] -> HR.Term primTy primVal
makeTuple [] = HR.Unit
makeTuple [t] = t
makeTuple (t : ts) = HR.Pair t (makeTuple ts)

transformArrow ::
  ReduceEff primTy primVal m =>
  NameSymbol.Mod ->
  Sexp.T ->
  m (HR.Term primTy primVal)
transformArrow q _f@(Sexp.List [π, xa, b]) =
  case xa of
    -- we never generate this form, so we should be able to erase this!?
    -- FE.NamedTypeE (FE.NamedType' x a) -> go π (getName x) a b
    a -> go π (pure "") a b
  where
    -- getName (Sexp.Atom Sexp.A {atomName}) =
    --   pure atomName
    -- -- don't these all get erased!?
    -- getName (name Sexp.:> _)
    --   | Sexp.isAtomNamed name ":implicit" = throwFF $ ImplicitsUnimplemented f
    -- getName _ = error "malformed transformArrow"
    go π x a b =
      HR.Pi <$> transformUsage q π
        <*> x
        <*> transformTermHR q a
        <*> transformTermHR q b
transformArrow _q _ = error "malfromed custom arrow"

transPrim ::
  ReduceEff primTy primVal m => Sexp.T -> m (HR.Term primTy primVal)
transPrim (Sexp.List [parm])
  | Just Sexp.A {atomName = p} <- Sexp.atomFromT parm = do
    param <- ask @"param"
    maybe (throwFF $ UnknownPrimitive p) pure $
      primTy param p <|> primVal param p
  where
    primTy param p = HR.PrimTy <$> HM.lookup p (P.builtinTypes param)
    primVal param p = HR.Prim <$> HM.lookup p (P.builtinValues param)
transPrim _ = error "malfromed prim"

pattern NamedArgTerm ::
  Symbol -> HR.Term primTy primVal -> HR.Term primTy primVal
pattern NamedArgTerm x ty <-
  HR.Elim (HR.Ann _ (HR.Elim (HR.Var (x :| []))) ty _)

transformSimpleLambda ::
  ( HasThrowFF primTy primVal m,
    HasParam primTy primVal m,
    HasCoreSigs primTy primVal m
  ) =>
  NameSymbol.Mod ->
  Sexp.T ->
  m (HR.Term primTy primVal)
transformSimpleLambda q (Sexp.List [args, body])
  | Just pats <- Sexp.toList args >>= NonEmpty.nonEmpty =
    foldr HR.Lam <$> transformTermHR q body <*> traverse isVarPat pats
transformSimpleLambda _ _ = error "malformed lambda"

transformSimpleLet ::
  ( HasThrowFF primTy primVal m,
    HasParam primTy primVal m,
    HasCoreSigs primTy primVal m
  ) =>
  NameSymbol.Mod ->
  Sexp.T ->
  m (HR.Term primTy primVal)
transformSimpleLet p e@(Sexp.List [name, Sexp.List [arg, cbody], body])
  | Just Sexp.A {atomName} <- Sexp.atomFromT name,
    Just xs <- Sexp.toList arg = do
    args <- traverse isVarArg xs
    cbody <- transformTermHR p cbody
    rhs <- toElim (Sexp.Cons (Sexp.atom ":let-match=") e) $ foldr HR.Lam cbody args
    HR.Let Usage.Omega atomName rhs <$> transformTermHR p body
transformSimpleLet _p (Sexp.List [_name, fun, _body]) =
  throwFF $ ExprUnimplemented fun
transformSimpleLet _ _ = error "malformed let"

isVarArg ::
  HasThrowFF primTy primVal m =>
  Sexp.T ->
  m NameSymbol.T
isVarArg p@(name Sexp.:> _rest)
  | Sexp.isAtomNamed name ":implicit-a" =
    throwFF $ PatternUnimplemented p
isVarArg p =
  isVarPat p
isVarArg _ = error "malformed arg"

isVarPat ::
  HasThrowFF primTy primVal m =>
  Sexp.T ->
  m NameSymbol.T
isVarPat p
  | Just Sexp.A {atomName} <- Sexp.atomFromT p =
    pure atomName
isVarPat p =
  throwFF $ PatternUnimplemented p

toElim ::
  HasThrowFF primTy primVal m =>
  -- | the original expression
  Sexp.T ->
  HR.Term primTy primVal ->
  m (HR.Elim primTy primVal)
toElim _ (HR.Elim e) = pure e
toElim e _ = throwFF $ NotAnElim e

getValSig ::
  ( HasCoreSigs primTy primVal m,
    HasThrowFF primTy primVal m
  ) =>
  NameSymbol.Mod ->
  NameSymbol.T ->
  m (IR.GlobalUsage, HR.Term primTy primVal)
getValSig q = getSig' q \case ValSig π ty -> Just (π, ty); _ -> Nothing

getConSig ::
  ( HasCoreSigs primTy primVal m,
    HasThrowFF primTy primVal m
  ) =>
  NameSymbol.Mod ->
  NameSymbol.T ->
  m (HR.Term primTy primVal, Maybe (Ctx.Def Sexp.T Sexp.T))
getConSig q = getSig' q \case
  ConSig (Just ty) def -> Just (ty, def)
  _ -> Nothing

getDataSig ::
  ( HasCoreSigs primTy primVal m,
    HasThrowFF primTy primVal m
  ) =>
  NameSymbol.Mod ->
  NameSymbol.T ->
  m (HR.Term primTy primVal, [NameSymbol.T])
getDataSig q = getSig' q \case DataSig ty cons -> Just (ty, cons); _ -> Nothing

getSig' ::
  ( HasCoreSigs primTy primVal m,
    HasThrowFF primTy primVal m
  ) =>
  NameSymbol.Mod ->
  (CoreSigHR primTy primVal -> Maybe a) ->
  NameSymbol.T ->
  m a
getSig' q f x = do
  msig <- lookupSig (Just q) x
  case msig of
    Just sig | Just ty <- f sig -> pure ty
    _ -> throwFF $ WrongSigType x msig

lookupSig ::
  HasCoreSigs primTy primVal m =>
  Maybe NameSymbol.Mod -> -- namespace of current declaration
  NameSymbol.T ->
  m (Maybe (CoreSig' HR.T primTy primVal))
lookupSig q x = fmap snd <$> lookupSig' q x

conDefName :: NameSymbol.T -> NameSymbol.T
conDefName = NameSymbol.applyBase (<> "$def")

transformType ::
  ( HasPatVars m,
    HasNextPatVar m,
    ReduceEff primTy primVal m
  ) =>
  NameSymbol.Mod ->
  NameSymbol.T ->
  a ->
  m [IR.RawGlobal primTy primVal]
transformType q name _ = do
  (ty, conNames) <- getDataSig q name
  let getConSig' x = do (ty, def) <- getConSig q x; pure (x, ty, def)
  conSigs <- traverse getConSig' conNames
  cons <- traverse (uncurry3 $ transformCon q) conSigs
  (args, ℓ) <- splitDataType name ty
  let dat' =
        IR.RawDatatype
          { rawDataName = name,
            rawDataArgs = args,
            rawDataLevel = ℓ,
            rawDataCons = cons,
            -- TODO ∷ replace
            rawDataPos = []
          }
  pure $ IR.RawGDatatype dat' : fmap IR.RawGDataCon cons

lookupSig' ::
  HasCoreSigs primTy primVal m =>
  Maybe NameSymbol.Mod -> -- namespace of current declaration
  NameSymbol.T ->
  m (Maybe (NameSymbol.T, CoreSig' HR.T primTy primVal))
lookupSig' q x' = do
  gets @"coreSigs" \sigs -> do
    let look x = (x,) <$> HM.lookup x sigs
    case q of
      Nothing -> look x
      Just q -> look x <|> look qx
        where
          qx = Ctx.removeTopName $ NameSymbol.qualify q x'
  where
    x = Ctx.removeTopName x'

splitDataType ::
  HasThrowFF primTy primVal m =>
  NameSymbol.T ->
  HR.Term primTy primVal ->
  m ([IR.RawDataArg primTy primVal], IR.Universe)
splitDataType x ty0 = go ty0
  where
    go (HR.Pi π x s t) = first (arg :) <$> splitDataType x t
      where
        arg =
          IR.RawDataArg
            { rawArgName = x,
              rawArgUsage = π,
              rawArgType = hrToIR s
            }
    go (HR.Star ℓ) = pure ([], ℓ)
    go _ = throwFF $ InvalidDatatypeType x ty0

transformFunction ::
  ( ReduceEff primTy primVal m,
    HasNextPatVar m,
    HasPatVars m
  ) =>
  NameSymbol.Mod ->
  NameSymbol.T ->
  Ctx.Def Sexp.T Sexp.T ->
  m (IR.RawFunction primTy primVal)
transformFunction q x (Ctx.D _ _ (_lambdaCase Sexp.:> defs) _)
  | Just xs <- Sexp.toList defs >>= NonEmpty.nonEmpty = do
    (π, typ) <- getValSig q x
    clauses <- traverse (transformClause q) xs
    pure $
      IR.RawFunction
        { rawFunName = x,
          rawFunUsage = π,
          rawFunType = hrToIR typ,
          rawFunClauses = clauses
        }
transformFunction _ _ _ = error "malformed defun"

transformUsage ::
  ( HasThrowFF primTy primVal m,
    HasCoreSigs primTy primVal m
  ) =>
  NameSymbol.Mod ->
  Sexp.T ->
  m Usage.T
transformUsage _ (Sexp.Atom Sexp.N {atomNum = i}) | i >= 0 = pure $ Usage.SNat $ fromInteger i
transformUsage q e = do
  o <- isOmega q e
  if o then pure Usage.Omega else throwFF $ NotAUsage e

transformSpecialRhs ::
  ( HasThrowFF primTy primVal m,
    HasCoreSigs primTy primVal m
  ) =>
  NameSymbol.Mod ->
  Sexp.T ->
  m (Maybe Special)
transformSpecialRhs _ (Sexp.List [name, prim])
  | Sexp.isAtomNamed name ":primitive",
    Just Sexp.A {atomName} <- Sexp.atomFromT prim =
    case atomName of
      "Builtin" :| ["Arrow"] -> pure $ Just $ ArrowS Nothing
      "Builtin" :| ["Pair"] -> pure $ Just $ PairS Nothing
      "Builtin" :| ["Omega"] -> pure $ Just OmegaS
      "Builtin" :| ["Colon"] -> pure $ Just ColonS
      "Builtin" :| ["Type"] -> pure $ Just TypeS
      "Builtin" :| (s : ss) -> throwFF $ UnknownBuiltin $ s :| ss
      _ -> pure Nothing
transformSpecialRhs q prim
  | Just Sexp.A {atomName} <- Sexp.atomFromT prim = getSpecial q atomName
transformSpecialRhs q (Sexp.List [f, arg])
  | Just Sexp.A {atomName} <- Sexp.atomFromT f =
    case show atomName of
      ':' : _ -> pure Nothing
      _ -> do
        head <- getSpecialE q f
        case head of
          Just (ArrowS Nothing) -> Just . ArrowS . Just <$> transformUsage q arg
          Just (PairS Nothing) -> Just . PairS . Just <$> transformUsage q arg
          _ -> pure Nothing
transformSpecialRhs _ _ = pure Nothing

transformSpecial ::
  ( HasThrowFF primTy primVal m,
    HasCoreSigs primTy primVal m
  ) =>
  NameSymbol.Mod ->
  Ctx.Definition Sexp.T Sexp.T Sexp.T ->
  m (Maybe Special)
transformSpecial q def@(Ctx.Def (Ctx.D π ty (Sexp.List [_, Sexp.List [Sexp.Nil, rhs]]) _)) = do
  rhs <- transformSpecialRhs q rhs
  when (isJust rhs) do
    unless (isNothing π) $ throwFF $ BuiltinWithUsage def
    unless (isNothing ty) $ throwFF $ BuiltinWithTypeSig def
  pure rhs
transformSpecial _ _ = pure Nothing

--------------------------------------------------------------------------------
-- Transform Signature
--------------------------------------------------------------------------------

transformSig ::
  ( 
    HasPatVars m,
    HasThrowFF primTy primVal m,
    HasParam primTy primVal m,
    HasCoreSigs primTy primVal m
  ) =>
  NameSymbol.T ->
  Ctx.Definition Sexp.T Sexp.T Sexp.T ->
  m [CoreSigHR primTy primVal]
transformSig x def = trySpecial <||> tryNormal
  where
    q = NameSymbol.mod x
    trySpecial = fmap SpecialSig <$> transformSpecial q def
    tryNormal = transformNormalSig q x def
    x <||> y = x >>= maybe y (pure . pure)

transformNormalSig ::
  (ReduceEff primTy primVal m, HasPatVars m, HasParam primTy primVal m) =>
  NameSymbol.Mod ->
  NameSymbol.T ->
  Ctx.Definition Sexp.T Sexp.T Sexp.T ->
  m [CoreSigHR primTy primVal]
transformNormalSig q x def@(Ctx.Def (Ctx.D π msig _ _)) =
  pure <$> transformValSig q x def π msig
transformNormalSig _ _ (Ctx.Record _) =
  pure [] -- TODO
transformNormalSig q x (Ctx.TypeDeclar typ) =
  transformTypeSig q x typ
transformNormalSig _ _ (Ctx.Unknown sig) =
  throwFF $ UnknownUnsupported (sig >>= eleToSymbol)
transformNormalSig q x (Ctx.SumCon Ctx.Sum {sumTDef}) = do
  let conSig = ConSig {conType = Nothing, conDef = sumTDef}
  let x' = conDefName x
  defSigs <- traverse (transformNormalSig q x' . Ctx.Def) sumTDef
  pure $ conSig : fromMaybe [] defSigs
transformNormalSig _ _ Ctx.CurrentNameSpace =
  pure []
transformNormalSig _ _ Ctx.Information {} =
  pure []

transformValSig ::
  ( 
    HasThrowFF primTy primVal m,
    HasParam primTy primVal m,
    HasCoreSigs primTy primVal m
  ) =>
  NameSymbol.Mod ->
  NameSymbol.T ->
  Ctx.Definition Sexp.T Sexp.T Sexp.T ->
  Maybe Usage.T ->
  Maybe Sexp.T ->
  m (CoreSigHR primTy primVal)
transformValSig q _ _ _ (Just (Sexp.List [usage, usageExpr, arrow]))
  | Sexp.isAtomNamed usage ":usage" =
    ValSig <$> transformGUsage q (Just usageExpr) <*> transformTermHR q arrow
transformValSig q _ _ _ (Just ty) =
  ValSig <$> transformGUsage q Nothing <*> transformTermHR q ty
transformValSig _ x def _ _ = throwFF $ SigRequired x def

transformDef ::
  (ReduceEff primTy primVal m, HasNextPatVar m, HasPatVars m) =>
  NameSymbol.T ->
  Ctx.Definition Sexp.T Sexp.T Sexp.T ->
  m [CoreDef primTy primVal]
transformDef x def = do
  sig <- lookupSig Nothing x
  case sig of
    Just (SpecialSig s) -> pure [SpecialDef x s]
    _ -> map CoreDef <$> transformNormalDef q x def
      where
        q = NameSymbol.mod x

transformNormalDef ::
  (ReduceEff primTy primVal m, HasNextPatVar m, HasPatVars m) =>
  NameSymbol.Mod ->
  NameSymbol.T ->
  Ctx.Definition Sexp.T Sexp.T Sexp.T ->
  m [IR.RawGlobal primTy primVal]
transformNormalDef q x (Ctx.TypeDeclar dec) = transformType q x dec
transformNormalDef _ _ Ctx.CurrentNameSpace = pure []
transformNormalDef _ _ Ctx.Information {} = pure []
transformNormalDef _ _ (Ctx.Unknown _) = pure []
transformNormalDef _ _ (Ctx.Record _) = pure [] -- TODO
transformNormalDef _ _ Ctx.SumCon {} = pure []
transformNormalDef q x (Ctx.Def def) = do
  f <- transformFunction q x def
  pure [IR.RawGFunction f]

transformUniverse ::
  HasThrowFF primTy primVal m =>
  Sexp.T ->
  m IR.Universe
transformUniverse (Sexp.Atom Sexp.N {atomNum = i}) | i >= 0 = pure $ fromIntegral i
transformUniverse e = throwFF $ NotAUniverse e

transformGUsage ::
  ( HasThrowFF primTy primVal m,
    HasCoreSigs primTy primVal m
  ) =>
  NameSymbol.Mod ->
  Maybe Sexp.T ->
  m IR.GlobalUsage
transformGUsage _ Nothing = pure IR.GOmega
transformGUsage _ (Just (Sexp.Atom Sexp.N {atomNum = 0})) = pure IR.GZero
transformGUsage q (Just e) = do
  o <- isOmega q e
  if o then pure IR.GOmega else throwFF $ NotAGUsage e

isOmega ::
  ( HasCoreSigs primTy primVal m,
    HasThrowFF primTy primVal m
  ) =>
  NameSymbol.Mod ->
  Sexp.T ->
  m Bool
isOmega q e = (== Just OmegaS) <$> getSpecialE q e

getSpecialE ::
  ( HasCoreSigs primTy primVal m,
    HasThrowFF primTy primVal m
  ) =>
  NameSymbol.Mod ->
  Sexp.T ->
  m (Maybe Special)
getSpecialE q x
  | Just Sexp.A {atomName} <- Sexp.atomFromT x = getSpecial q atomName
getSpecialE _ _ = pure Nothing

getSpecial ::
  ( HasCoreSigs primTy primVal m,
    HasThrowFF primTy primVal m
  ) =>
  NameSymbol.Mod ->
  NameSymbol.T ->
  m (Maybe Special)
getSpecial q x = do
  sig <- lookupSig (Just q) x
  case sig of
    Just (SpecialSig s) -> pure $ Just s
    Just _ -> pure Nothing
    Nothing -> throwFF $ WrongSigType x Nothing

------------------------------------------------------------
-- Transform Type signatures
------------------------------------------------------------
transformTypeSig ::
  (ReduceEff primTy primVal m, HasPatVars m, HasParam primTy primVal m) =>
  NameSymbol.Mod ->
  NameSymbol.T ->
  Sexp.T ->
  m [CoreSigHR primTy primVal]
transformTypeSig q name (nameAndData Sexp.:> args Sexp.:> typeForm)
  | Just typeArgs <- Sexp.toList args >>= traverse eleToSymbol = do
    (baseTy, hd) <- transformIndices typeArgs nameAndData
    let dataType = foldr makeTPi baseTy typeArgs
    (dataCons, conSigs) <- unzip <$> transformConSigs q hd typeForm
    let dataSig = DataSig {dataType, dataCons}
    pure $ dataSig : conSigs
  where
    transformIndices typeArgs (_ Sexp.:> grouped)
      | Just dataArrow <- Sexp.assoc (Sexp.atom ":type") grouped = do
        typ <- transformTermHR q dataArrow
        let hd0 = HR.Var name
        let args = HR.Elim . HR.Var . NameSymbol.fromSymbol <$> typeArgs
        pure (typ, Just $ HR.Elim $ foldl HR.App hd0 args)
    transformIndices _ _ =
      pure (HR.Star 0, Nothing) -- TODO metavar for universe
    makeTPi name res =
      -- TODO metavars for the named args instead of defaulting to types
      -- thin metavars for the named args instead of defaulting to types
      HR.Pi mempty (NameSymbol.fromSymbol name) (HR.Star 0) res
transformTypeSig _ _ _ = error "malformed type"

transformConSigs ::
  (ReduceEff primTy primVal m, HasPatVars m, HasParam primTy primVal m) =>
  -- | namespace containing declaration
  NameSymbol.Mod ->
  -- | datatype head
  Maybe (HR.Term primTy primVal) ->
  -- | rhs
  Sexp.T ->
  m [(NameSymbol.T, CoreSigHR primTy primVal)]
transformConSigs pfx hd =
  traverse (transformProduct pfx hd) <=< toProducts
  where
    -- We have a single constructor, which is a record
    toProducts (r@(record Sexp.:> _) Sexp.:> Sexp.Nil)
      | Sexp.isAtomNamed record ":record-d" =
        throwFF $ RecordUnimplemented r
    -- we can't have another standalone product here, so just send to
    -- sum
    toProducts sums
      | Just cons <- Sexp.toList sums =
        pure $ map toProduct1 cons
    toProducts _ = error "malfromed data constrcutor"
    toProduct1 (sumConstructor Sexp.:> value)
      | Just sumSym <- eleToSymbol sumConstructor =
        (sumSym, value)
    toProduct1 _ = error "malformed sum constructor"

transformCon ::
  ( ReduceEff primTy primVal m,
    HasPatVars m,
    HasNextPatVar m
  ) =>
  NameSymbol.Mod ->
  NameSymbol.T ->
  HR.Term primTy primVal ->
  Maybe (Ctx.Def Sexp.T Sexp.T) ->
  m (IR.RawDataCon primTy primVal)
transformCon q x ty def = do
  def <- traverse (transformFunction q (conDefName x)) def
  pure $
    IR.RawDataCon
      { rawConName = x,
        rawConType = hrToIR ty,
        rawConDef = def
      }

transformProduct ::
  ( HasPatVars m,
    HasThrowFF primTy primVal m,
    HasParam primTy primVal m,
    HasCoreSigs primTy primVal m
  ) =>
  NameSymbol.Mod ->
  -- | datatype head
  Maybe (HR.Term primTy primVal) ->
  (Symbol, Sexp.T) ->
  m (NameSymbol.T, CoreSigHR primTy primVal)
transformProduct q hd (x, prod) =
  (NameSymbol.qualify1 q x,) . makeSig
    <$> transformConSig q (NameSymbol.fromSymbol x) hd prod
  where
    makeSig ty = ConSig {conType = Just ty, conDef = Nothing}

transformArg ::
  ( HasNextPatVar m,
    HasPatVars m,
    HasThrowFF primTy primVal m,
    HasParam primTy primVal m
  ) =>
  Sexp.T ->
  m (IR.Pattern primTy primVal)
transformArg p@(name Sexp.:> _rest)
  | Sexp.isAtomNamed name ":implicit-a" =
    throwFF $ PatternUnimplemented p
transformArg pat = transformPat pat

transformClause ::
  (ReduceEff primTy primVal m, HasNextPatVar m, HasPatVars m) =>
  NameSymbol.Mod ->
  Sexp.T ->
  m (IR.RawFunClause primTy primVal)
transformClause q (Sexp.List [args', body])
  | Just args <- Sexp.toList args' = do
    put @"patVars" mempty
    put @"nextPatVar" 0
    patts <- traverse transformArg args
    clauseBody <- transformTermIR q body
    pure $ IR.RawFunClause [] patts clauseBody False
transformClause _ _ = error "malformed tansformClause"

transformConSig ::
  (ReduceEff primTy primVal m, HasPatVars m) =>
  NameSymbol.Mod ->
  NameSymbol.T ->
  Maybe (HR.Term primTy primVal) ->
  Sexp.T ->
  m (HR.Term primTy primVal)
transformConSig q name mHd r@((t Sexp.:> ts) Sexp.:> _)
  | named ":record-d" = throwFF $ RecordUnimplemented r
  | named ":arrow" = transformTermHR q ts
  | isNothing mHd =
    throwFF $ InvalidConstructor name r
  where
    named = Sexp.isAtomNamed t
transformConSig q name mHd r@(t Sexp.:> _ts)
  | isNothing mHd =
    throwFF $ InvalidConstructor name r
  | Just hd <- mHd,
    Just xs <- Sexp.toList r =
    let makeArr (x, arg) res =
          HR.Pi (Usage.SNat 1) x <$> transformTermHR q arg <*> pure res
        names = makeFieldName <$> [0 ..]
        makeFieldName i = NameSymbol.fromText $ "$field" <> show (i :: Int)
     in foldrM makeArr hd $ zip names xs
  where
    named = Sexp.isAtomNamed t
transformConSig _ _ _ _r = error "malformed transformConSig"

transformPat ::
  ( HasNextPatVar m,
    HasPatVars m,
    HasThrowFF primTy primVal m,
    HasParam primTy primVal m
  ) =>
  Sexp.T ->
  m (IR.Pattern primTy primVal)
transformPat p@(asCon Sexp.:> con)
  -- implicit arguments are not supported
  -- TODO ∷ translate as patterns into @let@
  | Sexp.isAtomNamed asCon ":as" =
    throwFF $ PatternUnimplemented p
  | Just args <- Sexp.toList con,
    Just Sexp.A {atomName} <- Sexp.atomFromT asCon =
    IR.PCon atomName <$> traverse transformPat args
transformPat n
  | Just x <- eleToSymbol n = do
    var <- getNextPatVar
    modify @"patVars" $ HM.insert (NameSymbol.fromSymbol x) var
    pure $ IR.PVar var
  | Just n@Sexp.N {} <- Sexp.atomFromT n =
    IR.PPrim <$> paramConstant n
  | otherwise = error "malformed match pattern"

getNextPatVar :: HasNextPatVar m => m IR.PatternVar
getNextPatVar = state @"nextPatVar" \v -> (v, succ v)

--------------------------------------------------------------------------------
-- General Helpers
--------------------------------------------------------------------------------
eleToSymbol :: Sexp.T -> Maybe Symbol
eleToSymbol x
  | Just Sexp.A {atomName} <- Sexp.atomFromT x =
    Just (NameSymbol.toSymbol atomName)
  | otherwise = Nothing
