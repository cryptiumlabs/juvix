module Juvix.ToCore.FromFrontend.Transform.Helpers 
( ReduceEff
, getParamConstant
, lookupSig
, lookupSigWithSymbol
, checkSymbolSig
, getValSig
, getConSig
, getDataSig
, isOmega
, getSpecialSig
, parseVarArg
, parseVarPat
, toElim
, splitDataType
, conDefName
, eleToSymbol
) where

import qualified Data.HashMap.Strict as HM
import qualified Juvix.Core.Common.Context as Ctx
import qualified Juvix.Core.HR as HR
import qualified Juvix.Core.IR as IR
import qualified Juvix.Core.Parameterisation as P
import Juvix.Core.Translate (hrToIR)
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.Sexp as Sexp
import Juvix.ToCore.Types
import Debug.Pretty.Simple (pTraceShow)




-- | Retrieve constant primVal from parameterization
getParamConstant ::
  (HasParam primTy primVal m, HasThrowFF primTy primVal m) =>
  Sexp.Atom ->
  m primVal
getParamConstant atom = do
  p <- ask @"param"
  case paramConstant' p atom of
    Just x -> pure x
    Nothing -> throwFF $ UnsupportedConstant (Sexp.Atom atom)
  where
    -- TODO: Maybe implement it for stringVal as well?
    paramConstant' p Sexp.N {atomNum} = P.intVal p atomNum
    paramConstant' _p Sexp.A {} = Nothing

-- | Lookup signature from a qualified symbol.
-- Returns the qualified symbol without "TopLevel." (in case it exists)
-- and the signature
lookupSigWithSymbol ::
  ( Show primTy, Show primVal,
  HasCoreSigs primTy primVal m) =>
  -- | Namespace of current declaration
  Maybe NameSymbol.Mod ->
  -- | Qualified symbol
  NameSymbol.T ->
  m (Maybe (NameSymbol.T, CoreSig HR.T primTy primVal))
lookupSigWithSymbol q x' = do
  gets @"coreSigs" \sigs -> do
    -- traceM "CoreSigs"
    -- pTraceShowM sigs
    case q of
      Nothing -> look x sigs
      Just q -> look x sigs <|> look qx sigs
        where
          qx = Ctx.removeTopName $ NameSymbol.qualify q x'
  where
    x = Ctx.removeTopName x'
    look a sigs = (a,) <$> HM.lookup a sigs

-- | Lookup signature from a qualified symbol.
lookupSig ::
  (Show primTy, Show primVal, HasCoreSigs primTy primVal m) =>
  Maybe NameSymbol.Mod -> -- namespace of current declaration
  NameSymbol.T ->
  m (Maybe (CoreSig HR.T primTy primVal))
lookupSig q x = fmap snd <$> lookupSigWithSymbol q x

-- | Lookup signature 
checkSymbolSig ::
  (Show primTy, Show primVal, ReduceEff primTy primVal m) =>
  NameSymbol.Mod ->
  NameSymbol.T ->
  m (HR.Term primTy primVal)
checkSymbolSig q symbol =
  toName <$> lookupSigWithSymbol (Just q) symbol
  where
    toName = HR.Elim . HR.Var . maybe symbol fst

isOmega ::
  ( Show primTy, Show primVal,
    HasCoreSigs primTy primVal m,
    HasThrowFF primTy primVal m
  ) =>
  NameSymbol.Mod ->
  Sexp.T ->
  m Bool
isOmega q e = (== Just OmegaS) <$> getSpecialSig q e

-- | Get special signature from atom
getSpecialSig ::
  ( Show primTy, Show primVal,
    HasCoreSigs primTy primVal m,
    HasThrowFF primTy primVal m
  ) =>
  NameSymbol.Mod ->
  Sexp.T ->
  m (Maybe Special)
getSpecialSig q x
  | Just Sexp.A {atomName} <- Sexp.atomFromT x = getSpecialSig' q atomName
  where
    getSpecialSig' q x = do
      sig <- lookupSig (Just q) x
      case sig of
        Just (SpecialSig s) -> pure $ Just s
        Just _ -> pure Nothing
        Nothing -> pTraceShow ("getSpecial", q, x) throwFF $ WrongSigType x Nothing
getSpecialSig _ _ = pure Nothing


-- | Check whether the S-expression form is a non-implicit string atom
parseVarArg ::
  HasThrowFF primTy primVal m =>
  Sexp.T ->
  m NameSymbol.T
parseVarArg p@(name Sexp.:> _rest)
  | Sexp.isAtomNamed name ":implicit-a" =
    throwFF $ PatternUnimplemented p
parseVarArg p = parseVarPat p

-- | Check whether the S-expression form is a string atom (i.e. not a number)
-- and return its name
parseVarPat ::
  HasThrowFF primTy primVal m =>
  Sexp.T ->
  m NameSymbol.T
parseVarPat p
  | Just Sexp.A {atomName} <- Sexp.atomFromT p =
    pure atomName
parseVarPat p =
  throwFF $ PatternUnimplemented p

-- | Unwrap Term
toElim ::
  HasThrowFF primTy primVal m =>
  -- | the original expression
  Sexp.T ->
  HR.Term primTy primVal ->
  m (HR.Elim primTy primVal)
toElim _ (HR.Elim e) = pure e
toElim e _ = throwFF $ NotAnElim e

getValSig ::
  ( Show primTy,
    Show primVal,
    HasCoreSigs primTy primVal m,
    HasThrowFF primTy primVal m
  ) =>
  NameSymbol.Mod ->
  NameSymbol.T ->
  m (IR.GlobalUsage, HR.Term primTy primVal)
getValSig q = getSig q \case ValSig π ty -> Just (π, ty); _ -> Nothing

getConSig ::
  ( Show primTy,
    Show primVal,
    HasCoreSigs primTy primVal m,
    HasThrowFF primTy primVal m
  ) =>
  NameSymbol.Mod ->
  NameSymbol.T ->
  m (HR.Term primTy primVal, Maybe (Ctx.Def Sexp.T Sexp.T))
getConSig q = getSig q \case
  ConSig (Just ty) def -> Just (ty, def)
  _ -> Nothing

getDataSig ::
  ( Show primTy,
    Show primVal,
    HasCoreSigs primTy primVal m,
    HasThrowFF primTy primVal m
  ) =>
  NameSymbol.Mod ->
  NameSymbol.T ->
  m (HR.Term primTy primVal, [NameSymbol.T])
getDataSig q = getSig q \case
  DataSig ty cons -> Just (ty, cons)
  _ -> Nothing

-- Lookup signature with a conditional
getSig ::
  ( Show a,
    Show primTy,
    Show primVal,
    HasCoreSigs primTy primVal m,
    HasThrowFF primTy primVal m
  ) =>
  NameSymbol.Mod ->
  (CoreSigHR primTy primVal -> Maybe a) ->
  NameSymbol.T ->
  m a
getSig q f x = do
  msig <- lookupSig (Just q) x
  case msig of
    Just sig | Just ty <- f sig -> pTraceShow ("lookupSig Success", q, x, msig, ty) $ pure ty
    _ -> pTraceShow ("lookupSig Failure", q, x, msig) $ throwFF $ WrongSigType x msig


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

conDefName :: NameSymbol.T -> NameSymbol.T
conDefName = identity -- NameSymbol.applyBase (<> "$def")

eleToSymbol :: Sexp.T -> Maybe Symbol
eleToSymbol x
  | Just Sexp.A {atomName} <- Sexp.atomFromT x =
    Just (NameSymbol.toSymbol atomName)
  | otherwise = Nothing

