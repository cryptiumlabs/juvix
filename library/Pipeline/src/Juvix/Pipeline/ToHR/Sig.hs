{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module Juvix.Pipeline.ToHR.Sig 

where

import Data.HashMap.Strict (HashMap)
import qualified Juvix.Core.Base.Types as Core
import Juvix.Library hiding (show)
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.Usage as Usage

import qualified Juvix.Context as Ctx
import qualified Juvix.Core.HR as HR
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.Usage as Usage
import qualified Juvix.Sexp as Sexp
import Juvix.Pipeline.ToHR.Term (transformTermHR)
import Juvix.Pipeline.ToHR.Env
import Juvix.Pipeline.ToHR.Sig.Extract

import Juvix.Pipeline.ToHR.TypeSig
  ( transformTypeSig,
  )
import Juvix.Pipeline.ToHR.Usage
  ( transformGUsage,
    transformUsage,
  )

hrToIRSig :: CoreSig HR.T ty val -> CoreSig IR.T ty val
hrToIRSig (CoreSig s)
hrToIRSig (SpecialSig s) = SpecialSig s

hrToIRSigs :: CoreSigs HR.T ty val -> CoreSigs IR.T ty val
hrToIRSigs sigs = hrToIRSig <$> sigs


transformSig ::
  ( HasPatVars m,
    HasThrowFF HR.T primTy primVal m,
    HasParam primTy primVal m,
    HasCoreSigs HR.T primTy primVal m,
    Show primTy,
    Show primVal
  ) =>
  NameSymbol.T ->
  Ctx.Definition Sexp.T Sexp.T Sexp.T ->
  m [CoreSig HR.T primTy primVal]
transformSig x def = trySpecial <||> tryNormal
  where
    q = NameSymbol.mod x
    trySpecial = fmap SpecialSig <$> transformSpecial q def
    tryNormal = transformNormalSig q x def
    x <||> y = x >>= maybe y (pure . pure)

extractDataConstructorSigs :: Sexp.T -> [Sexp.T]
extractDataConstructorSigs (typeCons Sexp.:> _ Sexp.:> dataCons)
  | Just dataConsL <- Sexp.toList dataCons =
    fmap
      (\n -> Sexp.cdr n Sexp.:> Sexp.car typeCons)
      dataConsL
extractDataConstructorSigs _t = []

transformNormalSig ::
  (ReduceEff HR.T primTy primVal m, HasPatVars m, HasParam primTy primVal m, Show primTy, Show primVal) =>
  NameSymbol.Mod ->
  NameSymbol.T ->
  Ctx.Definition Sexp.T Sexp.T Sexp.T ->
  m [CoreSig HR.T primTy primVal]
transformNormalSig q x def@(Ctx.Def (Ctx.D π msig _ _)) =
  pure <$> transformValSig q x def π msig
transformNormalSig _ _ (Ctx.Record record) =
  panic $ "Record not implemented" <> show record
-- pure [] -- TODO
transformNormalSig q x (Ctx.TypeDeclar typ) = do
  transformTypeSig q x typ
transformNormalSig _ _ (Ctx.Unknown sig) =
  throwFF $ UnknownUnsupported (sig >>= eleToSymbol)
transformNormalSig q x (Ctx.SumCon Ctx.Sum {sumTDef}) = do
  -- TODO: Lookup constructor signature from the context (consig)
  -- Use that type to check sumTDef
  let x' = conDefName x
  defSigs <- traverse (transformNormalSig q x' . Ctx.Def) sumTDef
  conSig <- conSigM
  pure $ conSig : fromMaybe [] defSigs
  where
    conSigM = case sumTDef of
      Just Ctx.D {defMTy} -> do
        ConSig <$> traverse (transformTermHR q) defMTy
      _ -> pure $ ConSig {conType = Nothing}
transformNormalSig _ _ Ctx.CurrentNameSpace =
  pure []
transformNormalSig _ _ Ctx.Information {} =
  pure []

transformValSig ::
  ( HasThrowFF HR.T primTy primVal m,
    HasParam primTy primVal m,
    HasCoreSigs HR.T primTy primVal m,
    Show primTy,
    Show primVal
  ) =>
  NameSymbol.Mod ->
  NameSymbol.T ->
  Ctx.Definition Sexp.T Sexp.T Sexp.T ->
  Maybe Usage.T ->
  Maybe Sexp.T ->
  m (CoreSig HR.T primTy primVal)
transformValSig q _ _ _ (Just (Sexp.List [usage, usageExpr, arrow]))
  | Sexp.isAtomNamed usage ":usage" =
    ValSig <$> transformGUsage q (Just usageExpr) <*> transformTermHR q arrow
transformValSig q _ _ _ (Just ty) =
  ValSig <$> transformGUsage q Nothing <*> transformTermHR q ty
transformValSig _ x def _ _ = throwFF $ SigRequired x def

transformSpecial ::
  ( Show primTy,
    Show primVal,
    HasThrowFF ext primTy primVal m,
    HasCoreSigs ext primTy primVal m
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

transformSpecialRhs ::
  ( Show primTy,
    Show primVal,
    HasThrowFF ext primTy primVal m,
    HasCoreSigs ext primTy primVal m
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
  | Just a@Sexp.A {} <- Sexp.atomFromT prim = getSpecialSig q (Sexp.Atom a)
transformSpecialRhs q (Sexp.List [f, arg])
  | Just Sexp.A {atomName} <- Sexp.atomFromT f =
    case show atomName of
      ':' : _ -> pure Nothing
      _ -> do
        head <- getSpecialSig q f
        case head of
          Just (ArrowS Nothing) -> Just . ArrowS . Just <$> transformUsage q arg
          Just (PairS Nothing) -> Just . PairS . Just <$> transformUsage q arg
          _ -> pure Nothing
transformSpecialRhs _ _ = pure Nothing

conDefName :: NameSymbol.T -> NameSymbol.T
conDefName = identity -- NameSymbol.applyBase (<> "$def")



