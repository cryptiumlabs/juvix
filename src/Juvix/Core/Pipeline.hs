{-# LANGUAGE LiberalTypeSynonyms #-}

module Juvix.Core.Pipeline where

import qualified Data.HashMap.Strict as HM
import qualified Juvix.Backends.Michelson as Michelson
import qualified Juvix.Core.Application as App
import qualified Juvix.Core.ErasedAnn as ErasedAnn
import qualified Juvix.Core.ErasedAnn.Prim as Prim
import qualified Juvix.Core.Erasure as Erasure
import qualified Juvix.Core.HR as HR
import qualified Juvix.Core.IR as IR
import qualified Juvix.Core.IR.Typechecker.Types as Typed
import qualified Juvix.Core.Translate as Translate
import qualified Juvix.Core.Types as Types
import Juvix.Library
import qualified Juvix.Library.Usage as Usage
import qualified Michelson.TypeCheck as Michelson
import qualified Michelson.Untyped as Michelson

type RawMichelson f = f Michelson.PrimTy Michelson.RawPrimVal

type RawMichelsonTerm = RawMichelson IR.Term

type Michelson f = f Michelson.PrimTy Michelson.PrimVal

type MichelsonTerm = Michelson IR.Term

type MichelsonComp res =
  forall m.
  MichelsonCompConstraints m =>
  RawMichelsonTerm ->
  Usage.T ->
  RawMichelsonTerm ->
  m res

type MichelsonCompConstraints m =
  ( HasWriter "log" [Types.PipelineLog Michelson.PrimTy Michelson.RawPrimVal] m,
    HasReader "parameterisation" (Types.Parameterisation Michelson.PrimTy Michelson.RawPrimVal) m,
    HasThrow "error" (Types.PipelineError Michelson.PrimTy Michelson.RawPrimVal Michelson.CompErr) m,
    HasReader "globals" (IR.Globals Michelson.PrimTy Michelson.PrimVal) m
  )

eraseGlobals ::
  MichelsonCompConstraints m =>
  m (Erasure.Globals Michelson.PrimTy Michelson.PrimVal)
eraseGlobals = do
  globals <- ask @"globals"
  res <- for (HM.toList globals) \(key, value) ->
    Erasure.eraseGlobal value
      |> Erasure.exec
      |> either (throw @"error" . Types.ErasureError) (pure . (key,))
  pure (HM.fromList res)

coreToAnn :: MichelsonComp (ErasedAnn.AnnTerm Michelson.PrimTy Michelson.PrimVal)
coreToAnn term usage ty = do
  -- FIXME: allow any universe!
  (term, _) <- typecheckErase' term usage ty
  pure $ ErasedAnn.convertTerm term usage

coreToMichelson :: MichelsonComp (Either Michelson.CompErr Michelson.EmptyInstr)
coreToMichelson term usage ty = do
  ann <- coreToAnn term usage ty
  pure $ fst $ Michelson.compileExpr $ toRaw ann

coreToMichelsonContract :: MichelsonComp (Either Michelson.CompErr (Michelson.Contract' Michelson.ExpandedOp, Michelson.SomeContract))
coreToMichelsonContract term usage ty = do
  ann <- coreToAnn term usage ty
  pure $ fst $ Michelson.compileContract $ toRaw ann

-- TODO: use typed terms in michelson backend
toRaw :: Michelson.Term -> Michelson.RawTerm
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
      ErasedAnn.AppM (takeToRaw fun) (takeToRaw <$> args)
    takeToRaw :: Michelson.Take -> Michelson.RawTerm
    takeToRaw (App.Take {usage, type', term}) =
      ErasedAnn.Ann
        { usage,
          type' = Prim.fromPrimType type',
          term = ErasedAnn.Prim term
        }

-- For interaction net evaluation, includes elementary affine check
-- , requires MonadIO for Z3.
-- FIXME
-- typecheckAffineErase ::
--   ( HasWriter "log" [Types.PipelineLog primTy primVal] m,
--     HasReader "parameterisation" (Types.Parameterisation primTy primVal) m,
--     HasThrow "error" (Types.PipelineError primTy primVal compErr) m,
--     HasReader "globals" (IR.Globals primTy primVal) m,
--     MonadIO m,
--     Eq primTy,
--     Eq primVal,
--     Show primTy,
--     Show primVal,
--     Show compErr
--   ) =>
--   HR.Term primTy primVal ->
--   Usage.T ->
--   HR.Term primTy primVal ->
--   m (Types.TermAssignment primTy primVal compErr)
-- typecheckAffineErase term usage ty = do
--   -- First typecheck & generate erased core.
--   (Types.WithType termAssign _type') <- typecheckErase term usage ty
--   -- Fetch the parameterisation, needed for EAC inference
--   -- TODO ∷ get rid of this dependency.
--   parameterisation <- ask @"parameterisation"
--   -- Then invoke Z3 to check elementary-affine-ness.
--   start <- liftIO unixTime
--   result <- liftIO (EAC.validEal parameterisation termAssign)
--   end <- liftIO unixTime
--   tell @"log" [Types.LogRanZ3 (end - start)]
--   -- Return accordingly.
--   case result of
--     Right (eac, _) -> do
--       let erasedEac = EAC.erase eac
--       unless
--         (erasedEac == Types.term termAssign)
--         ( throw @"error"
--             ( Types.InternalInconsistencyError
--                 "erased affine core should always match erased core"
--             )
--         )
--       pure termAssign
--     Left err -> throw @"error" (Types.EACError err)

-- For standard evaluation, no elementary affine check, no MonadIO required.
typecheckEval ::
  ( HasWriter "log" [Types.PipelineLog primTy primVal] m,
    HasReader "parameterisation" (Types.Parameterisation primTy primVal) m,
    HasThrow "error" (Types.PipelineError primTy primVal compErr) m,
    HasReader "globals" (IR.GlobalsT primTy primVal) m,
    Types.CanApply primTy,
    Types.CanApply (Typed.TypedPrim primTy primVal),
    Eq primTy,
    Eq primVal,
    Show primTy,
    Show primVal,
    Show compErr
  ) =>
  HR.Term primTy primVal ->
  Usage.T ->
  Typed.ValueT primTy primVal ->
  m (Typed.ValueT primTy primVal)
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
  ( HasWriter "log" [Types.PipelineLog primTy primVal] m,
    HasReader "parameterisation" (Types.Parameterisation primTy primVal) m,
    HasThrow "error" (Types.PipelineError primTy primVal compErr) m,
    HasReader "globals" (IR.GlobalsT primTy primVal) m,
    Types.CanApply primTy,
    Types.CanApply (Typed.TypedPrim primTy primVal),
    Eq primTy,
    Eq primVal,
    Show primTy,
    Show primVal,
    Show compErr
  ) =>
  IR.Term primTy primVal ->
  Usage.T ->
  IR.Term primTy primVal ->
  m (Erasure.TermT primTy primVal, Typed.ValueT primTy primVal)
typecheckErase' term usage ty = do
  -- TODO can we avoid the irToHR conversion
  ty <- typecheckEval (Translate.irToHR ty) (Usage.SNat 0) (IR.VStar 0)
  term <- typecheckErase term usage ty
  pure (term, ty)

-- For standard evaluation, no elementary affine check, no MonadIO required.
typecheckErase ::
  ( HasWriter "log" [Types.PipelineLog primTy primVal] m,
    HasReader "parameterisation" (Types.Parameterisation primTy primVal) m,
    HasThrow "error" (Types.PipelineError primTy primVal compErr) m,
    HasReader "globals" (IR.GlobalsT primTy primVal) m,
    Types.CanApply primTy,
    Types.CanApply (Typed.TypedPrim primTy primVal),
    Eq primTy,
    Eq primVal,
    Show primTy,
    Show primVal,
    Show compErr
  ) =>
  IR.Term primTy primVal ->
  Usage.T ->
  Typed.ValueT primTy primVal ->
  m (Erasure.TermT primTy primVal)
typecheckErase term usage ty = do
  -- Fetch the parameterisation, needed for typechecking.
  param <- ask @"parameterisation"
  globals <- ask @"globals"
  -- Typecheck & return accordingly.
  case IR.typeTerm param term (IR.Annotation usage ty)
    |> IR.execTC globals
    |> fst of
    Right tyTerm -> do
      case Erasure.erase tyTerm usage of
        Right res -> pure res
        Left err -> throw @"error" (Types.ErasureError err)
    Left err -> throw @"error" (Types.TypecheckerError err)
