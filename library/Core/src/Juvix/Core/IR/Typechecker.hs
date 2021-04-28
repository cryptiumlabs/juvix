-- | This file contains the functions and aux functions to typecheck
-- datatype and function declarations.
-- Datatype declarations are typechecked by @checkDataType@ in CheckDataType.hs.
-- Function declarations are typechecked by @typeCheckFuns@ in CheckFunction.hs.
-- Typechecked declarations are added to the signature.
module Juvix.Core.IR.Typechecker
  ( module Juvix.Core.IR.Typechecker,
    module Typed,
    module Env,
  )
where

import Juvix.Core.IR.CheckDatatype
import qualified Juvix.Core.IR.Evaluator as Eval
import qualified Juvix.Core.IR.TransformExt.OnlyExts as OnlyExts
import Juvix.Core.IR.Typechecker.Env as Env
import Juvix.Core.IR.Typechecker.Types as Typed
import qualified Juvix.Core.IR.Types as IR
import Juvix.Core.IR.Types.Base as IR
import qualified Juvix.Core.IR.Types.Globals as IR
import qualified Juvix.Core.Parameterisation as Param
import Juvix.Library hiding (Datatype)

typeCheckDeclaration ::
  ( Param.CanApply primTy,
    Param.CanApply primVal,
    Eval.CanEval extT IR.NoExt primTy primVal,
    Eval.HasPatSubstTerm (OnlyExts.T IR.NoExt) primTy primVal primTy,
    Eval.HasPatSubstTerm (OnlyExts.T IR.NoExt) primTy primVal primVal,
    Eq primTy,
    Eq primVal,
    IR.ValueAll Eq extV primTy primVal,
    IR.NeutralAll Eq extV primTy primVal,
    CanTC' extT primTy primVal m,
    Param.CanApply (TypedPrim primTy primVal),
    HasThrow "typecheckError" (TypecheckError' extV extT primTy primVal) m,
    HasThrow "typecheckError" (TypecheckError' extV IR.NoExt primTy primVal) m,
    HasReader "globals" (GlobalsT primTy primVal) (IR.TypeCheck ext primTy primVal m),
    HasThrow "typecheckError" (TypecheckError' extV extT primTy primVal) (IR.TypeCheck ext primTy primVal m),
    HasThrow "typecheckError" (TypecheckError' extV IR.NoExt primTy primVal) (IR.TypeCheck ext primTy primVal m),
    HasThrow "typecheckError" (TypecheckError' IR.NoExt extT primTy primVal) (IR.TypeCheck ext primTy primVal m)
  ) =>
  IR.Telescope extV extT primTy primVal ->
  IR.RawTelescope extT primTy primVal ->
  -- | The targeted parameterisation
  Param.Parameterisation primTy primVal ->
  [IR.RawDatatype' extT primTy primVal] ->
  [IR.RawFunction' ext primTy primVal] ->
  IR.TypeCheck ext primTy primVal m [IR.RawGlobal' extT primTy primVal]
typeCheckDeclaration tel rtel param [] [] =
  return []
typeCheckDeclaration tel rtel param dts fns =
  case dts of
    (hdd@(IR.RawDatatype name lpos args levels cons) : tld) ->
      do
        _ <- checkDataType tel name param args
        rest <- typeCheckDeclaration tel rtel param tld fns
        -- TODO checkedCons <- typeCheckAllCons param tel lpos rtel globals cons
        return $ IR.RawGDatatype hdd : rest-- <> checkedCons
    _ -> do
      return []
-- add to sig once typechecked
-- put $ addSig sig n (DataSig params pos sz v)
-- mapM_ (typeCheckConstructor n sz pos tel) cs
typeCheckDeclaration tel rtel param _ ((IR.RawFunction name usage ty cls) : tlf) =
  undefined

-- TODO run typeCheckFuns
