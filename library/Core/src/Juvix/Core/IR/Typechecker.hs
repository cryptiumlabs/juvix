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
import Juvix.Core.IR.Typechecker.Env as Env
import Juvix.Core.IR.Typechecker.Types as Typed
import qualified Juvix.Core.IR.Types as IR
import qualified Juvix.Core.IR.Types.Globals as IR
import qualified Juvix.Core.Parameterisation as Param
import Juvix.Library hiding (Datatype)

-- | type check datatype and function declarations
typeCheckDeclaration ::
  ( Eq primTy,
    Eq primVal,
    Param.CanApply primTy,
    Param.CanApply primVal,
    Eval.CanEval extT IR.NoExt primTy primVal,
    Eval.EvalPatSubst IR.NoExt primTy primVal,
    Eval.EvalPatSubst IR.NoExt primTy (TypedPrim primTy primVal),
    Eval.NoExtensions extT primTy (TypedPrim primTy primVal),
    Eval.NoExtensions extT primTy primVal,
    CanTC' extT primTy primVal m,
    Param.CanApply (TypedPrim primTy primVal),
    HasThrow "typecheckError" (TypecheckError' IR.NoExt extT primTy primVal) m,
    HasReader "globals" (GlobalsT' IR.NoExt extT primTy primVal) m
  ) =>
  -- | Telescope containing a list of
  -- (name, usage, ty (of type Value') and the extension)
  IR.Telescope IR.NoExt extT primTy primVal ->
  -- | Raw telescope containing ty (of type Term')
  IR.RawTelescope extT primTy primVal ->
  -- | The targeted parameterisation
  Param.Parameterisation primTy primVal ->
  -- | A list of datatype declarations to be checked
  [IR.RawDatatype' extT primTy primVal] ->
  -- | A list of function declarations to be checked
  [IR.RawFunction' extT primTy primVal] ->
  -- | A list of Globals to be added to the global state
  Env.TypeCheck IR.NoExt primTy primVal m [IR.RawGlobal' extT primTy primVal]
typeCheckDeclaration _tel _rtel _param [] [] =
  return []
-- type checking datatype declarations
typeCheckDeclaration tel rtel param dts fns =
  case dts of
    (hdd@(IR.RawDatatype name lpos args _levels cons) : tld) ->
      do
        globals <- lift $ ask @"globals"
        -- check the first datatype's args
        _ <- lift $ checkDataType tel name param args
        -- recurse the rest of the datatypes
        rest <- typeCheckDeclaration tel rtel param tld fns
        -- check all the constructors of the first datatype
        checkedCons <- typeCheckAllCons param tel lpos rtel globals cons
        -- when successful, return the datatype and the datacons
        -- to the list of globals
        return $ IR.RawGDatatype hdd : rest <> checkedCons
    _ -> do
      return []
-- TODO add to sig once typechecked? Keeping track of all globals may be
-- enough?
-- TODO type checking function declarations
-- typeCheckDeclaration tel rtel param _ (IR.RawFunction name usage ty cls : tlf) =
--   undefined
