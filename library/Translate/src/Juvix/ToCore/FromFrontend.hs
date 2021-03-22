{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}

module Juvix.ToCore.FromFrontend where

import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty as NonEmpty
import qualified Generics.SYB as SYB
import qualified Juvix.Core.Common.Context as Ctx
import qualified Juvix.Core.HR as HR
import qualified Juvix.Core.IR as IR
import qualified Juvix.Core.IR.Types as IR
import qualified Juvix.Core.Parameterisation as P
import Juvix.Core.Translate (hrToIR)
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.Sexp as Sexp
import qualified Juvix.Library.Usage as Usage
import Juvix.ToCore.Types
import Prelude (error)

type ReduceEff primTy primVal m =
  ( Data primTy,
    Data primVal,
    HasThrowFF primTy primVal m,
    HasParam primTy primVal m,
    HasCoreSigs primTy primVal m
  )

-- P.stringVal p s
-- P.floatVal p d

paramConstant' ::
  P.Parameterisation primTy primVal ->
  Sexp.Atom ->
  Maybe primVal
paramConstant' p Sexp.N {atomNum} = P.intVal p atomNum
paramConstant' _p Sexp.A {} = Nothing

transformTermIR ::
  ( Data primTy,
    Data primVal,
    HasPatVars m,
    HasThrowFF primTy primVal m,
    HasParam primTy primVal m,
    HasCoreSigs primTy primVal m
  ) =>
  NameSymbol.Mod ->
  Sexp.T ->
  m (IR.Term primTy primVal)
transformTermIR q fe = do
  SYB.everywhereM (SYB.mkM transformPatVar) . hrToIR =<< transformTermHR q fe

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
  | named ":record-no-pun" = throwFF $ RecordUnimplemented p
  | named ":refinement" = throwFF $ RefinementsUnimplemented p
  | named ":let-type" = throwFF $ ExprUnimplemented p
  | named ":list" = throwFF $ ListUnimplemented p
  | named "case" = throwFF $ ExprUnimplemented p
  | named ":u" = throwFF $ UniversesUnimplemented p
  -- Rest
  | named ":custom-arrow" = undefined
  | named ":let-match" = transformSimpleLet q form
  | named ":primitive" = transPrim form
  | named ":lambda" = transformSimpleLambda q form
  | named ":progn" = transformTermHR q (Sexp.car form)
  | named ":paren" = transformTermHR q (Sexp.car form)
  where
    named = Sexp.isAtomNamed name

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

transformSimpleLambda ::
  ( Data primTy,
    Data primVal,
    HasThrowFF primTy primVal m,
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
  ( Data primTy,
    Data primVal,
    HasThrowFF primTy primVal m,
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
isVarPat (Sexp.List [x])
  | Just Sexp.A {atomName} <- Sexp.atomFromT x =
    pure atomName
isVarPat p =
  throwFF $ PatternUnimplemented p

transformPat p@(asCon Sexp.:> con)
  -- implicit arguments are not supported
  | Sexp.isAtomNamed asCon ":as" =
    throwFF $ PatternUnimplemented p
  | otherwise =
    undefined

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
            rawDataCons = cons
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

------------------------------------------------------------
-- Transform Type signatures
------------------------------------------------------------

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
  ( Data primTy,
    Data primVal,
    HasPatVars m,
    HasThrowFF primTy primVal m,
    HasParam primTy primVal m,
    HasCoreSigs primTy primVal m
  ) =>
  NameSymbol.Mod ->
  Maybe (HR.Term primTy primVal) -> -- ^ datatype head
  (Symbol, Sexp.T) ->
  m (NameSymbol.T, CoreSigHR primTy primVal)
transformProduct q hd (x, prod) =
    (NameSymbol.qualify1 q x,) . makeSig <$>
    transformConSig q (NameSymbol.fromSymbol x) hd prod
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
transformConSig q name mHd r@(t Sexp.:> ts)
  | named ":record-d" = throwFF $ RecordUnimplemented r
  | named ":arrow" = transformTermHR q ts
  | isNothing mHd = throwFF $ InvalidConstructor name r
  | Just hd <- mHd,
    Just xs <- Sexp.toList r =
      let makeArr (x, arg) res =
                  HR.Pi (Usage.SNat 1) x <$> transformTermHR q arg <*> pure res
          names = makeFieldName <$> [0..]
          makeFieldName i = NameSymbol.fromText $ "$field" <> show (i :: Int)
      in
          foldrM makeArr hd $ zip names xs
  where
    named = Sexp.isAtomNamed t
transformConSig _ _ _ _ = error "malformed transformConSig"
--------------------------------------------------------------------------------
-- General Helpers
--------------------------------------------------------------------------------
eleToSymbol :: Sexp.T -> Maybe Symbol
eleToSymbol x
  | Just Sexp.A {atomName} <- Sexp.atomFromT x =
    Just (NameSymbol.toSymbol atomName)
  | otherwise = Nothing
