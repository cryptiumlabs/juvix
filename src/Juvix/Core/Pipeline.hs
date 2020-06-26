module Juvix.Core.Pipeline where

import qualified Data.Text as Text
--import qualified Juvix.Core.EAC as EAC

import qualified Juvix.Backends.Michelson as Michelson
import qualified Juvix.Core.Erased.Datatypes as Datatypes
import qualified Juvix.Core.ErasedAnn as ErasedAnn
import qualified Juvix.Core.Erasure as Erasure
import qualified Juvix.Core.HR as HR
import qualified Juvix.Core.IR as IR
import qualified Juvix.Core.Translate as Translate
import qualified Juvix.Core.Types as Types
import qualified Juvix.Core.Usage as Usage
import Juvix.Library

-- For interaction net evaluation, includes elementary affine check
-- , requires MonadIO for Z3.
{-
typecheckAffineErase ::
  ( HasWriter "log" [Types.PipelineLog primTy primVal] m,
    HasReader "parameterisation" (Types.Parameterisation primTy primVal) m,
    HasThrow "error" (Types.PipelineError primTy primVal compErr) m,
    HasReader "globals" (IR.Globals primTy primVal) m,
    MonadIO m,
    Eq primTy,
    Eq primVal,
    Show primTy,
    Show primVal,
    Show compErr
  ) =>
  HR.Term primTy primVal ->
  Usage.T ->
  HR.Term primTy primVal ->
  m (Types.TermAssignment primTy primVal compErr)
typecheckAffineErase term usage ty = do
  -- First typecheck & generate erased core.
  (Types.WithType termAssign _type') <- typecheckErase term usage ty
  -- Fetch the parameterisation, needed for EAC inference
  -- TODO ∷ get rid of this dependency.
  parameterisation <- ask @"parameterisation"
  -- Then invoke Z3 to check elementary-affine-ness.
  start <- liftIO unixTime
  result <- liftIO (EAC.validEal parameterisation termAssign)
  end <- liftIO unixTime
  tell @"log" [Types.LogRanZ3 (end - start)]
  -- Return accordingly.
  case result of
    Right (eac, _) -> do
      let erasedEac = EAC.erase eac
      unless
        (erasedEac == Types.term termAssign)
        ( throw @"error"
            ( Types.InternalInconsistencyError
                "erased affine core should always match erased core"
            )
        )
      pure termAssign
    Left err -> throw @"error" (Types.EACError err)
-}

coreToMichelson ::
  ( HasWriter "log" [Types.PipelineLog Michelson.PrimTy Michelson.PrimVal] m,
    HasReader "parameterisation" (Types.Parameterisation Michelson.PrimTy Michelson.PrimVal) m,
    HasThrow "error" (Types.PipelineError Michelson.PrimTy Michelson.PrimVal Michelson.CompErr) m,
    HasReader "globals" (IR.Globals Michelson.PrimTy Michelson.PrimVal) m
  ) =>
  HR.Term Michelson.PrimTy Michelson.PrimVal ->
  Usage.T ->
  HR.Term Michelson.PrimTy Michelson.PrimVal ->
  m (Either Michelson.CompErr Michelson.EmptyInstr)
coreToMichelson term usage ty = do
  Types.WithType (Types.Assignment term _) ty <- typecheckErase term usage ty
  globals <- ask @"globals"
  converted <- pure (Datatypes.datatypesToMichelson globals term)
  ann <- ErasedAnn.convertTerm converted usage ty
  let (res, _) = Michelson.compileExpr ann
  pure res

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
  m (Types.AssignWithType primTy primVal compErr)
typecheckErase term usage ty = do
  -- Fetch the parameterisation, needed for typechecking.
  param <- ask @"parameterisation"
  globals <- ask @"globals"
  -- First convert HR to IR.
  let irTerm = Translate.hrToIR term
  let irType = Translate.hrToIR ty
  tell @"log" [Types.LogHRtoIR term irTerm]
  tell @"log" [Types.LogHRtoIR ty irType]
  let (Right irTypeValue, _) = IR.exec globals (IR.evalTerm param irType)
  -- Typecheck & return accordingly.
  case IR.typeTerm param 0 [] irTerm (IR.Annotation usage irTypeValue)
    |> IR.exec globals
    |> fst of
    Right _ -> do
      -- TODO convert to HRAnn
      --case Erasure.erase globals param term usage ty of
      case Erasure.erase globals param undefined usage undefined of
        Right res -> pure res
        Left err -> throw @"error" (Types.ErasureError err)
    Left err -> throw @"error" (Types.TypecheckerError (Text.pack (show err)))
