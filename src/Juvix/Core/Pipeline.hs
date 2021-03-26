{-# LANGUAGE LiberalTypeSynonyms #-}

module Juvix.Core.Pipeline
  ( coreToAnn,
    toRaw,
  )
where

import qualified Juvix.Core.Application as App
import qualified Juvix.Core.ErasedAnn as ErasedAnn
import qualified Juvix.Core.ErasedAnn.Prim as Prim
import qualified Juvix.Core.Erasure as Erasure
import qualified Juvix.Core.HR as HR
import qualified Juvix.Core.IR as IR
import qualified Juvix.Core.IR.Typechecker as TC
import qualified Juvix.Core.Translate as Translate
import qualified Juvix.Core.Types as Types
import Juvix.Library
import qualified Juvix.Library.Usage as Usage

type RawTerm ty val = IR.Term ty val

type RawElim ty val = IR.Elim ty val

type Term ty val = HR.Term ty val

type Comp ty val res =
  forall m.
  CompConstraints ty val res m =>
  RawTerm ty val ->
  Usage.T ->
  RawTerm ty val ->
  m res

type CompConstraints' primTy primVal compErr m =
  ( HasWriter "log" [Types.PipelineLog primTy primVal] m,
    HasReader "parameterisation" (Types.Parameterisation primTy primVal) m,
    HasThrow "error" (Types.PipelineError primTy primVal compErr) m,
    HasReader "globals" (IR.Globals primTy (Types.TypedPrim primTy primVal)) m
  )

type CompConstraints primTy primVal compErr m =
  ( CompConstraints' primTy primVal compErr m,
    Eq primTy,
    Eq primVal,
    Types.CanApply primTy,
    Types.CanApply (Types.TypedPrim primTy primVal),
    TC.PrimSubstValue primTy primVal,
    TC.PrimPatSubstTerm primTy primVal,
    IR.HasWeak primVal
  )

constMapPrim :: Erasure.MapPrim a a ty val
constMapPrim _ x = Right x

lookupMapPrim ::
  Erasure.MapPrim
    (Types.TypedPrim ty val)
    (ErasedAnn.TypedPrim ty val)
    ty
    (Types.TypedPrim ty val)
lookupMapPrim _ (App.Return ty tm) = pure $ App.Return ty tm
lookupMapPrim ns (App.Cont f xs n) =
  App.Cont f <$> traverse lookupArg xs <*> pure n
  where
    lookupArg (App.BoundArg i) =
      atMay ns (fromIntegral i)
        |> maybe (error i) (pure . App.VarArg)
    lookupArg (App.FreeArg x) = pure $ App.VarArg x
    lookupArg (App.TermArg t) = pure $ App.TermArg t
    error i =
      Left
        $ Erasure.InternalError
        $ "unknown de Bruijn index " <> show i

coreToAnn :: Comp ty val (ErasedAnn.AnnTerm ty (App.Return' ErasedAnn.T (NonEmpty ty) val))
coreToAnn term usage ty = do
  -- FIXME: allow any universe!
  (term, _) <- typecheckErase' term usage ty
  pure $ ErasedAnn.convertTerm term usage

-- For standard evaluation, no elementary affine check, no MonadIO required.
typecheckEval ::
  CompConstraints primTy primVal compErr m =>
  HR.Term primTy primVal ->
  Usage.T ->
  IR.Value primTy (Types.TypedPrim primTy primVal) ->
  m (IR.Value primTy (Types.TypedPrim primTy primVal))
typecheckEval term usage ty = do
  -- Fetch the parameterisation, needed for typechecking.
  param <- ask @"parameterisation"
  globals <- ask @"globals"
  -- First convert HR to IR.
  let irTerm = Translate.hrToIR term
  tell @"log" [Types.LogHRtoIR term irTerm]
  -- Typecheck & return accordingly.
  case IR.typeTerm param irTerm (IR.Annotation usage ty)
    >>= IR.evalTC
    |> IR.execTC globals
    |> fst of
    Right value -> pure value
    Left err -> throw @"error" (Types.TypecheckerError err)

-- For standard evaluation, no elementary affine check, no MonadIO required.
typecheckErase' ::
  CompConstraints primTy primVal compErr m =>
  IR.Term primTy primVal ->
  Usage.T ->
  IR.Term primTy primVal ->
  m
    ( Erasure.Term primTy (ErasedAnn.TypedPrim primTy primVal),
      IR.Value primTy (Types.TypedPrim primTy primVal)
    )
typecheckErase' term usage ty = do
  ty <- typecheckEval (Translate.irToHR ty) (Usage.SNat 0) (IR.VStar 0)
  term <- typecheckErase term usage ty
  pure (term, ty)

-- For standard evaluation, no elementary affine check, no MonadIO required.
typecheckErase ::
  CompConstraints primTy primVal compErr m =>
  IR.Term primTy primVal ->
  Usage.T ->
  IR.Value primTy (Types.TypedPrim primTy primVal) ->
  m (Erasure.Term primTy (ErasedAnn.TypedPrim primTy primVal))
typecheckErase term usage ty = do
  -- Fetch the parameterisation, needed for typechecking.
  param <- ask @"parameterisation"
  globals <- ask @"globals"
  -- Typecheck & return accordingly.
  case IR.typeTerm param term (IR.Annotation usage ty)
    |> IR.execTC globals
    |> fst of
    Right tyTerm -> do
      case Erasure.erase constMapPrim lookupMapPrim tyTerm usage of
        Right res -> pure res
        Left err -> throw @"error" (Types.ErasureError err)
    Left err -> throw @"error" (Types.TypecheckerError err)

toRaw :: ErasedAnn.AnnTerm ty (App.Return' ErasedAnn.T (NonEmpty ty) val) -> ErasedAnn.AnnTerm ty val
toRaw t@(ErasedAnn.Ann {term}) = t {ErasedAnn.term = toRaw1 term}
  where
    toRaw1 (ErasedAnn.Var x) = ErasedAnn.Var x
    toRaw1 (ErasedAnn.Prim p) = primToRaw p
    toRaw1 t@(ErasedAnn.LamM {body}) = t {ErasedAnn.body = toRaw body}
    toRaw1 (ErasedAnn.PairM l r) = ErasedAnn.PairM (toRaw l) (toRaw r)
    toRaw1 ErasedAnn.UnitM = ErasedAnn.UnitM
    toRaw1 (ErasedAnn.AppM f xs) = ErasedAnn.AppM (toRaw f) (toRaw <$> xs)
    primToRaw (App.Return {retTerm}) = ErasedAnn.Prim retTerm
    primToRaw (App.Cont {fun, args}) =
      ErasedAnn.AppM (takeToTerm fun) (argsToTerms (App.type' fun) args)
    takeToTerm (App.Take {usage, type', term}) =
      ErasedAnn.Ann
        { usage,
          type' = Prim.fromPrimType type',
          term = ErasedAnn.Prim term
        }
    argsToTerms ts xs = go (toList ts) xs
      where
        go _ [] = []
        go (_ : ts) (App.TermArg a : as) =
          takeToTerm a : go ts as
        go (t : ts) (App.VarArg x : as) =
          varTerm t x : go ts as
        go [] (_ : _) =
          -- a well typed application can't have more arguments than arrows
          undefined
        varTerm t x =
          ErasedAnn.Ann
            { usage = Usage.Omega, -- FIXME should usages even exist after erasure?
              type' = ErasedAnn.PrimTy t,
              term = ErasedAnn.Var x
            }
