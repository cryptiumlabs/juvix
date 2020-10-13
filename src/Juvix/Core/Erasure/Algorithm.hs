module Juvix.Core.Erasure.Algorithm (erase, eraseAnn, eraseGlobal, Erasure.exec) where

import Data.List (genericIndex)
import qualified Juvix.Core.Erased.Types as Erased
import qualified Juvix.Core.Erasure.Types as Erasure
import qualified Juvix.Core.IR as IR
import qualified Juvix.Core.IR.Typechecker.Types as Typed
import qualified Juvix.Core.Usage as Usage
import Juvix.Library hiding (empty)

type ErasureM primTy primVal m =
  ( HasState "nextName" Int m,
    HasState "nameStack" [Symbol] m,
    HasThrow "erasureError" (Erasure.Error primTy primVal) m
  )

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

eraseGlobal ::
  ErasureM primTy primVal m =>
  IR.Global primTy primVal ->
  m (Erasure.Global primTy primVal)
eraseGlobal g =
  case g of
    IR.GDatatype g -> Erasure.GDatatype |<< eraseDatatype g
    IR.GDataCon c -> Erasure.GDataCon |<< eraseDataCon c
    IR.GFunction f -> Erasure.GFunction |<< eraseFunction f
    -- TODO: Need the annotated term here. ref https://github.com/metastatedev/juvix/issues/495
    IR.GAbstract u t -> Erasure.GAbstract u |<< eraseTerm undefined

eraseDatatype ::
  ErasureM primTy primVal m =>
  IR.Datatype primTy primVal ->
  m (Erasure.Datatype primTy)
eraseDatatype (IR.Datatype name args level cons) = do
  args <- mapM eraseDataArg args
  cons <- mapM eraseDataCon cons
  pure (Erasure.Datatype name args level cons)

eraseDataArg ::
  ErasureM primTy primVal m =>
  IR.DataArg primTy primVal ->
  m (Erasure.DataArg primTy)
eraseDataArg (IR.DataArg name usage ty isParam) = do
  ty <- eraseType ty
  pure (Erasure.DataArg name usage ty isParam)

eraseDataCon ::
  ErasureM primTy primVal m =>
  IR.DataCon primTy primVal ->
  m (Erasure.DataCon primTy)
eraseDataCon (IR.DataCon name ty) = do
  ty <- eraseType ty
  pure (Erasure.DataCon name ty)

eraseFunction ::
  ErasureM primTy primVal m =>
  IR.Function primTy primVal ->
  m (Erasure.Function primTy primVal)
eraseFunction (IR.Function name usage ty clauses) = do
  let (tys, ret) = piTypeToList (IR.quote0 ty)
  clauses <- flip mapM clauses $ \(IR.FunClause patts term) -> do
    let ty_ret = listToPiType (drop (length patts) tys, ret)
    (patts, ty) <- erasePatterns (patts, (tys, ret))
    patts <- mapM erasePattern patts
    -- TODO: Need the annotated term here. ref https://github.com/metastatedev/juvix/issues/495
    -- term <- eraseTerm term
    pure (Erasure.FunClause patts undefined)
  ty <- eraseType ty
  pure (Erasure.Function name usage ty clauses)

eraseFunClause ::
  ErasureM primTy primVal m =>
  IR.FunClause primTy primVal ->
  m (Erasure.FunClause primTy primVal)
eraseFunClause (IR.FunClause patts term) = do
  patts <- mapM erasePattern patts
  -- TODO: Need the annotated term here. ref https://github.com/metastatedev/juvix/issues/495
  -- term <- eraseTerm term
  pure (Erasure.FunClause patts undefined)
  undefined

erasePattern ::
  ErasureM primTy primVal m =>
  IR.Pattern primTy primVal ->
  m (Erasure.Pattern primTy primVal)
erasePattern patt =
  case patt of
    IR.PCon name patts -> do
      patts <- mapM erasePattern patts
      pure (Erasure.PCon name patts)
    IR.PVar v -> pure (Erasure.PVar v)
    IR.PDot t -> do
      -- TODO: Need the annotated term here. ref https://github.com/metastatedev/juvix/issues/495
      -- t <- eraseTerm t
      -- pure (Erasure.PDot t)
      pure (Erasure.PDot undefined)
    IR.PPrim p -> pure (Erasure.PPrim p)

erasePatterns ::
  ErasureM primTy primVal m =>
  ([IR.Pattern primTy primVal], ([(Usage.Usage, IR.Term primTy primVal)], IR.Term primTy primVal)) ->
  m ([IR.Pattern primTy primVal], ([(Usage.Usage, IR.Term primTy primVal)], IR.Term primTy primVal))
erasePatterns ([], ([], ret)) = pure ([], ([], ret))
erasePatterns (p : ps, ((Usage.SNat 0, _) : args, ret)) = erasePatterns (ps, (args, ret))
erasePatterns (p : ps, (arg : args, ret)) = do
  (ps', (args', ret')) <- erasePatterns (ps, (args, ret))
  pure (p : ps', (arg : args', ret'))
erasePatterns _ = throw @"erasureError" (Erasure.InternalError "invalid type & pattern match combination")

piTypeToList :: IR.Term primTy primVal -> ([(Usage.Usage, IR.Term primTy primVal)], IR.Term primTy primVal)
piTypeToList ty =
  case ty of
    IR.Pi usage arg ret ->
      let (rest, res) = piTypeToList ret in ((usage, arg) : rest, res)
    _ -> ([], ty)

listToPiType :: ([(Usage.Usage, IR.Term primTy primVal)], IR.Term primTy primVal) -> IR.Term primTy primVal
listToPiType ([], ret) = ret
listToPiType ((u, x) : xs, ret) = IR.Pi u x (listToPiType (xs, ret))

eraseTerm ::
  ErasureM primTy primVal m =>
  Typed.Term primTy primVal ->
  m (Erasure.Term primTy primVal)
eraseTerm t@(Typed.Star _ _) = throwEra $ Erasure.UnsupportedTermT t
eraseTerm t@(Typed.PrimTy _ _) = throwEra $ Erasure.UnsupportedTermT t
eraseTerm (Typed.Prim p ann) = do
  Erasure.Prim p <$> eraseType (IR.annType ann)
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
      let exprTy = IR.annType $ IR.baResAnn anns
          bindTy = IR.annType $ IR.baBindAnn anns
      b <- eraseElim b
      bindTy <- eraseType bindTy
      exprTy <- eraseType exprTy
      pure (Erasure.Let x b t (bindTy, exprTy))
eraseTerm (Typed.Elim e _) = eraseElim e

eraseElim ::
  ErasureM primTy primVal m =>
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
  ErasureM primTy primVal m =>
  IR.Value primTy primVal ->
  m (Erasure.Type primTy)
eraseType (IR.VStar i) = do
  pure $ Erasure.Star i
eraseType (IR.VPrimTy t) = do
  pure $ Erasure.PrimTy t
eraseType (IR.VPi π a b) = do
  if π == mempty
    then eraseType b
    else -- FIXME dependency

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
  ErasureM primTy primVal m =>
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
