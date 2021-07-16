module Juvix.Pipeline.ToHR.Sig.Extract where


import Data.HashMap.Strict (HashMap)
import qualified Juvix.Core.Base.Types as Core
import Juvix.Library hiding (show)
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.Usage as Usage
import qualified Juvix.Library.HashMap as HM
import qualified Juvix.Context as Ctx
import qualified Juvix.Core.HR as HR
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.Usage as Usage
import qualified Juvix.Sexp as Sexp

import Juvix.Pipeline.ToHR.Env
import Juvix.Pipeline.ToHR.Types



------------------------------------------------------------
-- Misc Helpers
------------------------------------------------------------

getValSig ::
  ( Show primTy,
    Show primVal,
    Core.CoreShow ext primTy primVal,
    HasCoreSigs ext primTy primVal m,
    HasThrowFF ext primTy primVal m
  ) =>
  NameSymbol.Mod ->
  NameSymbol.T ->
  m (Core.GlobalUsage, Core.Term' ext primTy primVal)
getValSig q = getSig q \case 
    CoreSig (Core.ValSig π ty) -> Just (π, ty)
    _ -> Nothing

getConSig ::
  ( Show primTy,
    Show primVal,
    Core.CoreShow ext primTy primVal,
    HasCoreSigs ext primTy primVal m,
    HasThrowFF ext primTy primVal m
  ) =>
  NameSymbol.Mod ->
  NameSymbol.T ->
  m (Core.Term' ext primTy primVal)
getConSig q = getSig q \case
  CoreSig (Core.ConSig (Just ty)) -> Just ty
  _ -> Nothing

getDataSig ::
  ( Show primTy,
    Show primVal,
    Core.CoreShow ext primTy primVal,
    HasCoreSigs ext primTy primVal m,
    HasThrowFF ext primTy primVal m
  ) =>
  NameSymbol.Mod ->
  NameSymbol.T ->
  m (Core.Term' ext primTy primVal, [NameSymbol.T])
getDataSig q = getSig q \case
  CoreSig (Core.DataSig ty cons) -> Just (ty, cons)
  _ -> Nothing

-- Lookup signature with a conditional
getSig ::
  ( Show a,
    Show primTy,
    Show primVal,
    HasCoreSigs ext primTy primVal m,
    HasThrowFF ext primTy primVal m
  ) =>
  NameSymbol.Mod ->
  (CoreSig ext primTy primVal -> Maybe a) ->
  NameSymbol.T ->
  m a
getSig q f x = do
  msig <- lookupSig (Just q) x
  case msig of
    Just sig | Just ty <- f sig -> pure ty
    _ -> throwFF $ WrongSigType x msig

-- | Get special signature from atom
getSpecialSig ::
  ( Show primTy,
    Show primVal,
    HasCoreSigs ext primTy primVal m,
    HasThrowFF ext primTy primVal m
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
        Nothing -> throwFF $ WrongSigType x Nothing
getSpecialSig _ _ = pure Nothing

-- | Lookup signature from a qualified symbol.
-- Returns the qualified symbol without "TopLevel." (in case it exists)
-- and the signature
lookupSigWithSymbol ::
  ( Show primTy,
    Show primVal,
    HasCoreSigs ext primTy primVal m
  ) =>
  -- | Namespace of current declaration
  Maybe NameSymbol.Mod ->
  -- | Qualified symbol
  NameSymbol.T ->
  m (Maybe (NameSymbol.T, CoreSig ext primTy primVal))
lookupSigWithSymbol q x' = do
  gets @"coreSigs" \sigs -> do
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
  (Show primTy, Show primVal, HasCoreSigs ext primTy primVal m) =>
  Maybe NameSymbol.Mod -> -- namespace of current declaration
  NameSymbol.T ->
  m (Maybe (CoreSig ext primTy primVal))
lookupSig q x = fmap snd <$> lookupSigWithSymbol q x

-- | Lookup signature
checkSymbolSig ::
  (Show primTy, Show primVal, ReduceEff ext primTy primVal m) =>
  NameSymbol.Mod ->
  NameSymbol.T ->
  m (HR.Term primTy primVal)
checkSymbolSig q symbol =
  toName <$> lookupSigWithSymbol (Just q) symbol
  where
    toName = HR.Elim . HR.Var . maybe symbol fst

