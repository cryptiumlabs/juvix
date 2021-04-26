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

import Juvix.Core.IR.Typechecker.Env as Env
import Juvix.Core.IR.Typechecker.Types as Typed
import Juvix.Core.IR.Types (Global')
import qualified Juvix.Core.IR.Types as IR
import qualified Juvix.Core.IR.Types.Globals as IR
import qualified Juvix.Core.Parameterisation as Param
import Juvix.Library hiding (Datatype)
import Juvix.Core.IR.CheckDatatype

typeCheckDeclaration ::
  -- | The targeted parameterisation
  Param.Parameterisation primTy primVal ->
  [IR.RawDatatype' ext primTy primVal] ->
  [IR.RawFunction' ext primTy primVal] ->
  IR.TypeCheck ext primTy primVal m [IR.RawGlobal' extT primTy primVal]
typeCheckDeclaration param [] [] =
  return []
typeCheckDeclaration param dts fns =
  case dts of
    (dtHd@(IR.RawDatatype name lpos args levels cons) : tld) ->
      do
        _ <- checkDataType tel name param args
        rest <- typeCheckDeclaration param tld fns
        return IR.RawGDatatype dtHd : rest
    _ -> do
      return []
-- v <- eval [] dt
-- add to sig once typechecked
-- put $ addSig sig n (DataSig params pos sz v)
-- mapM_ (typeCheckConstructor n sz pos tel) cs
typeCheckDeclaration param _ ((IR.RawFunction name usage ty cls) : tlf) =
  undefined

-- TODO run typeCheckFuns
