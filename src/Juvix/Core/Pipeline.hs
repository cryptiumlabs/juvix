module Juvix.Core.Pipeline where

import qualified Data.Text as Text
import qualified Juvix.Core.EAC as EAC
import qualified Juvix.Core.Erased as EC
import qualified Juvix.Core.Erasure as Erasure
import qualified Juvix.Core.HR as HR
import qualified Juvix.Core.IR as IR
import qualified Juvix.Core.Translate as Translate
import qualified Juvix.Core.Types as Types
import Juvix.Core.Usage
import Juvix.Library

-- For interaction net evaluation, includes elementary affine check, requires MonadIO for Z3.
typecheckAffineErase ∷
  ∀ primTy primVal m.
  ( HasWriter "log" [Types.PipelineLog primTy primVal] m,
    HasReader "parameterisation" (Types.Parameterisation primTy primVal) m,
    HasThrow "error" (Types.PipelineError primTy primVal) m,
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
  ((erased, _), assignment) ← typecheckErase term usage ty
  -- Fetch the parameterisation, needed for EAC inference (TODO: get rid of this dependency).
  parameterisation ← ask @"parameterisation"
  -- Then invoke Z3 to check elementary-affine-ness.
  start ← liftIO unixTime
  result ← liftIO (EAC.validEal parameterisation erased assignment)
  end ← liftIO unixTime
  tell @"log" [Types.LogRanZ3 (end - start)]
  -- Return accordingly.
  case result of
    Right (eac, _) → do
      let erasedEac = EAC.erase eac
      unless
        (erasedEac == erased)
        ( throw @"error"
            ( Types.InternalInconsistencyError
                "erased affine core should always match erased core"
            )
        )
      pure (erased, assignment)
    Left err → throw @"error" (Types.EACError err)

-- For standard evaluation, no elementary affine check, no MonadIO required.
typecheckErase ∷
  ∀ primTy primVal m.
  ( HasWriter "log" [Types.PipelineLog primTy primVal] m,
    HasReader "parameterisation" (Types.Parameterisation primTy primVal) m,
    HasThrow "error" (Types.PipelineError primTy primVal) m,
    Eq primTy,
    Eq primVal,
    Show primTy,
    Show primVal
  ) ⇒
  HR.Term primTy primVal →
  Usage →
  HR.Term primTy primVal →
  m ((EC.Term primVal, EC.Type primTy), EC.TypeAssignment primTy)
typecheckErase term usage ty = do
  -- Fetch the parameterisation, needed for typechecking.
  parameterisation ← ask @"parameterisation"
  -- First convert HR to IR.
  let irTerm = Translate.hrToIR term
  let irType = Translate.hrToIR ty
  tell @"log" [Types.LogHRtoIR term irTerm]
  tell @"log" [Types.LogHRtoIR ty irType]
  let (Right irTypeValue, _) = IR.exec (IR.evalTerm parameterisation irType IR.initEnv)
  -- Typecheck & return accordingly.
  case fst (IR.exec (IR.typeTerm parameterisation 0 [] irTerm (usage, irTypeValue))) of
    Right () → do
      case Erasure.erase parameterisation term usage ty of
        Right res → pure res
        Left err → throw @"error" (Types.ErasureError err)
    Left err → throw @"error" (Types.TypecheckerError (Text.pack (show err)))
