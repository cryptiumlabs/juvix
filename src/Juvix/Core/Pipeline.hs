module Juvix.Core.Pipeline where

import qualified Data.Text as Text
import qualified Juvix.Core.EAC as EAC
import qualified Juvix.Core.Erased as EC
import Juvix.Core.Erasure
import qualified Juvix.Core.HR as HR
import qualified Juvix.Core.IR as IR
import Juvix.Core.Translate
import Juvix.Core.Types
import Juvix.Core.Usage
import Juvix.Library

-- For interaction net evaluation, includes elementary affine check, requires MonadIO for Z3.
typecheckAffineErase ∷
  ∀ primTy primVal m.
  ( HasWriter "log" [PipelineLog primTy primVal] m,
    -- TODO Should be HasReader
    HasState "parameterisation" (Parameterisation primTy primVal) m,
    HasThrow "error" (PipelineError primTy primVal) m,
    MonadIO m,
    Eq primTy,
    Eq primVal,
    Show primTy,
    Show primVal
  ) ⇒
  HR.Term primTy primVal →
  Usage →
  HR.Term primTy primVal →
  m (EC.Term primVal, EC.TypeAssignment primTy)
typecheckAffineErase term usage ty = do
  -- First typecheck & generate erased core.
  (erased, assignment) ← typecheckErase term usage ty
  -- Then invoke Z3 to check elementary-affine-ness.
  start ← liftIO unixTime
  result ← liftIO (EAC.validEal erased assignment)
  end ← liftIO unixTime
  tell @"log" [LogRanZ3 (end - start)]
  -- Return accordingly.
  case result of
    Right (eac, _) → do
      let erasedEac = EAC.erase eac
      unless (erasedEac == erased) (throw @"error" (InternalInconsistencyError "erased affine core should always match erased core"))
      pure (erased, assignment)
    Left err → throw @"error" (EACError err)

-- For standard evaluation, no elementary affine check, no MonadIO required.
typecheckErase ∷
  ∀ primTy primVal m.
  ( HasWriter "log" [PipelineLog primTy primVal] m,
    -- TODO Should be HasReader
    HasState "parameterisation" (Parameterisation primTy primVal) m,
    HasThrow "error" (PipelineError primTy primVal) m,
    Eq primTy,
    Eq primVal,
    Show primTy,
    Show primVal
  ) ⇒
  HR.Term primTy primVal →
  Usage →
  HR.Term primTy primVal →
  m (EC.Term primVal, EC.TypeAssignment primTy)
typecheckErase term usage ty = do
  -- Fetch the parameterisation, needed for typechecking.
  parameterisation ← get @"parameterisation"
  -- First convert HR to IR.
  let irTerm = hrToIR term
  tell @"log" [LogHRtoIR term irTerm]
  let irType = hrToIR ty
  tell @"log" [LogHRtoIR ty irType]
  let irTypeValue = IR.cEval parameterisation irType IR.initEnv
  -- Typecheck & return accordingly.
  case IR.cType parameterisation 0 [] irTerm (usage, irTypeValue) of
    Right () → do
      case erase term usage ty of
        Right res → pure res
        Left err → throw @"error" (ErasureError err)
    Left err → throw @"error" (TypecheckerError (Text.pack err))
