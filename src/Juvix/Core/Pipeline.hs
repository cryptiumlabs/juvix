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
  ( HasWriter "log" (PipelineLog primTy primVal) m,
    HasReader "parameterisation" (Parameterisation primTy primVal) m,
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
  (erased, assignment) ← typecheckErase term usage ty
  result ← liftIO (EAC.validEal erased assignment)
  case result of
    Right (eac, _) → do
      let erasedEac = EAC.erase eac
      unless (erasedEac == erased) (throw @"error" (InternalInconsistencyError "erased affine core should always match erased core"))
      pure (erased, assignment)
    Left err → throw @"error" (EACError err)

-- For standard evaluation, no elementary affine check, no MonadIO required.
typecheckErase ∷
  ∀ primTy primVal m.
  ( HasWriter "log" (PipelineLog primTy primVal) m,
    HasReader "parameterisation" (Parameterisation primTy primVal) m,
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
  parameterisation ← ask @"parameterisation"
  let irTerm = hrToIR term
      irType = hrToIR ty
      irTypeValue = IR.cEval parameterisation irType IR.initEnv
  case IR.cType parameterisation 0 [] irTerm (usage, irTypeValue) of
    Right () → do
      case erase term usage ty of
        Right res → pure res
        Left err → throw @"error" (ErasureError err)
    Left err → throw @"error" (TypecheckerError (Text.pack err))
