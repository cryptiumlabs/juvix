module Juvix.Core.Erasure.Algorithm (erase, eraseAnn) where

import Data.List (genericIndex)
import qualified Juvix.Core.Erased.Types as Erased
import qualified Juvix.Core.Erasure.Types as Erasure
import qualified Juvix.Core.IR as IR
import qualified Juvix.Core.IR.Typechecker.Types as Typed
import qualified Juvix.Core.Usage as Usage
import Juvix.Library hiding (empty)

eraseAnn :: Erasure.Term primTy primVal -> Erased.Term primVal
eraseAnn (Erasure.Var x _) = Erased.Var x
eraseAnn (Erasure.Prim p _) = Erased.Prim p
eraseAnn (Erasure.Lam x t _) = Erased.Lam x (eraseAnn t)
eraseAnn (Erasure.Let x b t _) = Erased.Let x (eraseAnn b) (eraseAnn t)
eraseAnn (Erasure.App s t _) = Erased.App (eraseAnn s) (eraseAnn t)

erase ::
  Typed.Term primTy primVal ->
  Usage.T ->
  Either (Erasure.Error primTy primVal) (Erasure.Term primTy primVal)
erase t π
  | π == mempty = Left $ Erasure.CannotEraseZeroUsageTerm t
  | otherwise = Erasure.exec $ eraseTerm t

eraseTerm ::
  ( HasState "nextName" Int m,
    HasState "nameStack" [Symbol] m,
    HasThrow "erasureError" (Erasure.Error primTy primVal) m
  ) =>
  Typed.Term primTy primVal ->
  m (Erasure.Term primTy primVal)
eraseTerm t@(Typed.Star _ _) = throwEra $ Erasure.UnsupportedTermT t
eraseTerm t@(Typed.PrimTy _ _) = throwEra $ Erasure.UnsupportedTermT t
eraseTerm t@(Typed.Pi _ _ _ _) = throwEra $ Erasure.UnsupportedTermT t
eraseTerm (Typed.Lam t anns) = do
  let ty@(IR.VPi π _ _) = IR.annType $ IR.baResAnn anns
  (x, t) <- withName \x -> (x,) <$> eraseTerm t
  if π == mempty
    then pure t
    else Erasure.Lam x t <$> eraseType ty
eraseTerm (Typed.Let π b t anns) = do
  (x, t) <- withName \x -> (x,) <$> eraseTerm t
  if π == mempty
    then pure t
    else do
      let ty = IR.annType $ IR.baResAnn anns
      b <- eraseElim b
      Erasure.Let x b t <$> eraseType ty
eraseTerm (Typed.Elim e _) = eraseElim e

eraseElim ::
  ( HasState "nextName" Int m,
    HasState "nameStack" [Symbol] m,
    HasThrow "erasureError" (Erasure.Error primTy primVal) m
  ) =>
  Typed.Elim primTy primVal ->
  m (Erasure.Term primTy primVal)
eraseElim (Typed.Bound x ann) = do
  Erasure.Var <$> lookupBound x
    <*> eraseType (IR.annType ann)
eraseElim (Typed.Free (IR.Global x) ann) = do
  Erasure.Var x <$> eraseType (IR.annType ann)
eraseElim e@(Typed.Free (IR.Pattern _) _) = do
  -- FIXME ??????
  throwEra $ Erasure.UnsupportedTermE e
eraseElim (Typed.Prim p ann) = do
  Erasure.Prim p <$> eraseType (IR.annType ann)
eraseElim (Typed.App e s ann) = do
  let IR.VPi π _ _ = IR.annType $ IR.getElimAnn e
  e <- eraseElim e
  if π == mempty
    then pure e
    else do
      s <- eraseTerm s
      Erasure.App e s <$> eraseType (IR.annType ann)
eraseElim (Typed.Ann _ s _ _ _) = do
  eraseTerm s

eraseType ::
  forall primTy primVal m.
  ( HasThrow "erasureError" (Erasure.Error primTy primVal) m,
    HasState "nameStack" [Symbol] m,
    HasState "nextName" Int m
  ) =>
  IR.Value primTy primVal ->
  m (Erasure.Type primTy)
eraseType (IR.VStar i) = do
  pure $ Erasure.Star i
eraseType (IR.VPrimTy t) = do
  pure $ Erasure.PrimTy t
eraseType (IR.VPi π a b) = do
  -- FIXME dependency
  Erasure.Pi π <$> eraseType a
    <*> withName \_ -> eraseType b
eraseType v@(IR.VLam _) = do
  throwEra $ Erasure.UnsupportedTypeV v
eraseType (IR.VNeutral n) = do
  eraseTypeN n
eraseType v@(IR.VPrim _) = do
  throwEra $ Erasure.UnsupportedTypeV v

eraseTypeN ::
  forall primTy primVal m.
  ( HasThrow "erasureError" (Erasure.Error primTy primVal) m,
    HasState "nameStack" [Symbol] m
  ) =>
  IR.Neutral primTy primVal ->
  m (Erasure.Type primTy)
eraseTypeN (IR.NBound x) = do
  Erasure.SymT <$> lookupBound x
eraseTypeN (IR.NFree (IR.Global x)) = do
  pure $ Erasure.SymT x
eraseTypeN n@(IR.NFree (IR.Pattern _)) = do
  -- FIXME ??????
  throwEra $ Erasure.UnsupportedTypeN n
eraseTypeN n@(IR.NApp _ _) = do
  -- FIXME add AppT and fill this in
  throwEra $ Erasure.UnsupportedTypeN n

pushName ::
  (HasState "nextName" Int m, HasState "nameStack" [Symbol] m) =>
  m Symbol
pushName = do
  x <- gets @"nextName" $ internText . show
  modify @"nextName" succ
  modify @"nameStack" (x :)
  pure $ x

popName ::
  ( HasState "nameStack" [a] m,
    HasThrow "erasureError" (Erasure.Error primTy primVal) m
  ) =>
  m ()
popName = do
  ns <- get @"nameStack"
  case ns of
    [] -> throw @"erasureError" $ Erasure.InternalError "name stack ran out"
    _ : ns -> put @"nameStack" ns

withName ::
  ( HasState "nextName" Int m,
    HasState "nameStack" [Symbol] m,
    HasThrow "erasureError" (Erasure.Error primTy primVal) m
  ) =>
  (Symbol -> m a) ->
  m a
withName f = do x <- pushName; f x <* popName

lookupBound ::
  HasState "nameStack" [Symbol] m =>
  IR.BoundVar ->
  m Symbol
lookupBound x = gets @"nameStack" (`genericIndex` x)

throwEra ::
  HasThrow "erasureError" (Erasure.Error primTy primVal) m =>
  Erasure.Error primTy primVal ->
  m a
throwEra = throw @"erasureError"
