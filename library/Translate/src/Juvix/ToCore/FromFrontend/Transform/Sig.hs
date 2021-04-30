module Juvix.ToCore.FromFrontend.Transform.Sig where

import qualified Data.HashMap.Strict as HM
import Debug.Pretty.Simple (pTraceShow, pTraceShowM)
import qualified Juvix.Core.Common.Context as Ctx
import qualified Juvix.Core.HR as HR
import qualified Juvix.Core.Parameterisation as P
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.Sexp as Sexp
import qualified Juvix.Library.Usage as Usage
import Juvix.ToCore.FromFrontend.Transform.HR (transformTermHR)
import Juvix.ToCore.FromFrontend.Transform.Helpers
  ( ReduceEff,
    conDefName,
    eleToSymbol,
    getSpecialSig,
  )
import Juvix.ToCore.FromFrontend.Transform.TypeSig
  ( transformTypeSig,
  )
import Juvix.ToCore.FromFrontend.Transform.Usage
  ( transformGUsage,
    transformUsage,
  )
import Juvix.ToCore.Types
  ( CoreSig (..),
    CoreSigHR,
    Error (..),
    HasCoreSigs,
    HasParam,
    HasPatVars,
    HasThrowFF,
    Special (..),
    throwFF,
  )

transformSig ::
  ( HasPatVars m,
    HasThrowFF primTy primVal m,
    HasParam primTy primVal m,
    HasCoreSigs primTy primVal m,
    Show primTy,
    Show primVal
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

extractDataConstructorSigs :: Sexp.T -> [Sexp.T]
extractDataConstructorSigs (typeCons Sexp.:> _ Sexp.:> dataCons)
  | Just dataConsL <- Sexp.toList dataCons =
    fmap
      (\n -> Sexp.cdr n Sexp.:> Sexp.car typeCons)
      dataConsL
extractDataConstructorSigs _t = []

transformNormalSig ::
  (ReduceEff primTy primVal m, HasPatVars m, HasParam primTy primVal m, Show primTy, Show primVal) =>
  NameSymbol.Mod ->
  NameSymbol.T ->
  Ctx.Definition Sexp.T Sexp.T Sexp.T ->
  m [CoreSigHR primTy primVal]
transformNormalSig q x def@(Ctx.Def (Ctx.D π msig _ _)) =
  pure <$> transformValSig q x def π msig
transformNormalSig _ _ (Ctx.Record record) =
  panic $ "Record not implemented" <> show record
-- pure [] -- TODO
transformNormalSig q x (Ctx.TypeDeclar typ) = do
  traceM "Type declaration!"
  pTraceShowM (x, typ)
  traceM "Extracted data constructor sig!"
  pTraceShowM $ extractDataConstructorSigs typ
  transformTypeSig q x typ
transformNormalSig _ _ (Ctx.Unknown sig) =
  throwFF $ UnknownUnsupported (sig >>= eleToSymbol)
transformNormalSig q x (Ctx.SumCon Ctx.Sum {sumTDef}) = do
  traceM "SumCon!"
  pTraceShowM (x, sumTDef)
  let x' = conDefName x
  defSigs <- traverse (transformNormalSig q x' . Ctx.Def) sumTDef
  traceM "DefSigs!"
  pTraceShowM (defSigs)

  conSig <- conSigM
  pure $ conSig : fromMaybe [] defSigs
  where
    conSigM = case sumTDef of
      Nothing -> pure $ ConSig {conType = Nothing}
      Just Ctx.D {defMTy} -> do
        ty <- mkTy defMTy
        pure $ ConSig (Just ty)

    -- data Bar = Foo Bool Int
    -- (-> Bool (-> Int Bar)
    -- Pi (SNat 1) ("Foo":[]) (1: Bool) (Pi (SNat 1) ("":[]) (Int) (Bar))
    --                      ^ input type
    mkTy (Just (fn Sexp.:> t1))
      | isFn fn =
        -- panic $ "mkTy not implemented" <> show (fn, t1, t2)
        mkTy (Just t1)
    mkTy (Just (Sexp.Atom Sexp.A {atomName = p} Sexp.:> fn Sexp.:> t2)) | isFn fn =
      do
        param <- ask @"param"
        traceM "mkty2"
        pTraceShowM (p, t2)
        HR.Pi (Usage.SNat 1) "" (primTy param p) <$> mkTy (Just t2)
    mkTy (Just e) = pTraceShow ("mkty e", e) transformTermHR q e
    mkTy Nothing = panic "Shouldn't happen"
    isFn fn = Sexp.isAtomNamed fn "TopLevel.Prelude.->"
    primTy param p = fromMaybe (panic $ "Can't lookup primTy " <> show p) (HR.PrimTy <$> HM.lookup p (P.builtinTypes param))
transformNormalSig _ _ Ctx.CurrentNameSpace =
  pure []
transformNormalSig _ _ Ctx.Information {} =
  pure []

transformValSig ::
  ( HasThrowFF primTy primVal m,
    HasParam primTy primVal m,
    HasCoreSigs primTy primVal m,
    Show primTy,
    Show primVal
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

transformSpecial ::
  ( Show primTy,
    Show primVal,
    HasThrowFF primTy primVal m,
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

transformSpecialRhs ::
  ( Show primTy,
    Show primVal,
    HasThrowFF primTy primVal m,
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
