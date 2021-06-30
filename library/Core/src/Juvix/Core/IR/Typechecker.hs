-- | This file contains the functions and aux functions to typecheck
-- datatype and function declarations.
-- Datatype declarations are typechecked by @checkDataType@ in CheckDataType.hs.
-- Function declarations are typechecked by @typeCheckFuns@ in CheckFunction.hs.
-- Typechecked declarations are added to the signature.
module Juvix.Core.IR.Typechecker
  ( module Juvix.Core.IR.Typechecker,
    module Typed,
    module Error,
    module Env,
  )
where

import qualified Juvix.Core.Base.Types as Core
import Juvix.Core.IR.CheckDatatype
import Juvix.Core.IR.CheckTerm (ShowExt)
import qualified Juvix.Core.IR.Evaluator as Eval
import Juvix.Core.IR.Typechecker.Env as Env
import Juvix.Core.IR.Typechecker.Error as Error
import Juvix.Core.IR.Typechecker.Types as Typed
import qualified Juvix.Core.IR.Types as IR
import qualified Juvix.Core.Parameterisation as Param
import Juvix.Library hiding (Datatype)

-- | type check datatype and function declarations
typeCheckDeclaration ::
  ( Eq primTy,
    Eq primVal,
    Show primTy,
    Show primVal,
    Show extT,
    ShowExt extT primTy primVal,
    Eval.CanEval extT IR.T primTy primVal,
    Eval.EvalPatSubst IR.T primTy primVal,
    Eval.EvalPatSubst IR.T primTy (Param.TypedPrim primTy primVal),
    Eval.NoExtensions extT primTy (Param.TypedPrim primTy primVal),
    Eval.NoExtensions extT primTy primVal,
    Env.CanTC' extT primTy primVal m,
    Param.CanApply (Param.TypedPrim primTy primVal),
    HasReader "globals" (Typed.GlobalsT' IR.T extT primTy primVal) m
  ) =>
  -- | Telescope containing a list of
  -- (name, usage, ty (of type Value') and the extension)
  Core.Telescope IR.T extT primTy primVal ->
  -- | Raw telescope containing ty (of type Term')
  Core.RawTelescope extT primTy primVal ->
  -- | The targeted parameterisation
  Param.Parameterisation primTy primVal ->
  -- | A list of datatype declarations to be checked
  [Core.RawDatatype' extT primTy primVal] ->
  -- | A list of function declarations to be checked
  [Core.RawFunction' extT primTy primVal] ->
  -- | A list of Globals to be added to the global state
  Env.TypeCheck IR.T primTy primVal m [Core.RawGlobal' extT primTy primVal]
typeCheckDeclaration _tel _rtel _param [] [] =
  return []
-- type checking datatype declarations
typeCheckDeclaration tel rtel param dts fns =
  case dts of
    (hdd@(Core.RawDatatype name lpos args _levels cons) : tld) ->
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
        return $ Core.RawGDatatype hdd : rest <> checkedCons
    [] -> do
      -- TODO functions, etc
      return []
