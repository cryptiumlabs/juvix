module Juvix.Core.Pipeline where

import qualified Juvix.Backends.Michelson as Michelson
import qualified Juvix.Core.ErasedAnn as ErasedAnn
import qualified Juvix.Core.Erasure as Erasure
import qualified Juvix.Core.HR as HR
import qualified Juvix.Core.IR as IR
import qualified Juvix.Core.Translate as Translate
import qualified Juvix.Core.Types as Types
import qualified Juvix.Core.Usage as Usage
import Juvix.Library
import qualified Michelson.TypeCheck as Michelson
import qualified Michelson.Untyped as Michelson

type MichelsonTerm = HR.Term Michelson.PrimTy Michelson.PrimVal

type MichelsonComp res =
  forall m.
  ( HasWriter "log" [Types.PipelineLog Michelson.PrimTy Michelson.PrimVal] m,
    HasReader "parameterisation" (Types.Parameterisation Michelson.PrimTy Michelson.PrimVal) m,
    HasThrow "error" (Types.PipelineError Michelson.PrimTy Michelson.PrimVal Michelson.CompErr) m,
    HasReader "globals" (IR.Globals Michelson.PrimTy Michelson.PrimVal) m
  ) =>
  MichelsonTerm ->
  Usage.T ->
  MichelsonTerm ->
  m (res)

coreToMichelson :: MichelsonComp (Either Michelson.CompErr Michelson.EmptyInstr)
coreToMichelson term usage ty = do
  term <- typecheckErase term usage ty
  ann <- ErasedAnn.convertTerm term usage
  -- TODO: Datatype & pattern matching compilation step will happen here.
  let (res, _) = Michelson.compileExpr ann
  pure res

coreToMichelsonContract :: MichelsonComp (Either Michelson.CompErr (Michelson.Contract' Michelson.ExpandedOp, Michelson.SomeContract))
coreToMichelsonContract term usage ty = do
  term <- typecheckErase term usage ty
  ann <- ErasedAnn.convertTerm term usage
  -- TODO: Datatype & pattern matching compilation step will happen here.
  let (res, _) = Michelson.compileContract ann
  pure res

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
typecheckErase ::
  ( HasWriter "log" [Types.PipelineLog primTy primVal] m,
    HasReader "parameterisation" (Types.Parameterisation primTy primVal) m,
    HasThrow "error" (Types.PipelineError primTy primVal compErr) m,
    HasReader "globals" (IR.Globals primTy primVal) m,
    Eq primTy,
    Eq primVal,
    Show primTy,
    Show primVal,
    Show compErr
  ) =>
  HR.Term primTy primVal ->
  Usage.T ->
  HR.Term primTy primVal ->
  m (Erasure.Term primTy primVal)
typecheckErase term usage ty = do
  -- Fetch the parameterisation, needed for typechecking.
  param <- ask @"parameterisation"
  globals <- ask @"globals"
  -- First convert HR to IR.
  let irTerm = Translate.hrToIR term
  let irType = Translate.hrToIR ty
  tell @"log" [Types.LogHRtoIR term irTerm]
  tell @"log" [Types.LogHRtoIR ty irType]
  let (Right irTypeValue, _) = IR.execTC globals (IR.evalTerm param irType)
  -- Typecheck & return accordingly.
  case IR.typeTerm param irTerm (IR.Annotation usage irTypeValue)
    |> IR.execTC globals
    |> fst of
    Right tyTerm -> do
      case Erasure.erase tyTerm usage of
        Right res -> pure res
        Left err -> throw @"error" (Types.ErasureError err)
    Left err -> throw @"error" (Types.TypecheckerError err)
