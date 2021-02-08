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

-- import qualified Data.IntMap.Strict as IntMap
-- import Data.List.NonEmpty ((<|))
-- import qualified Juvix.Core.Application as App
-- import qualified Juvix.Core.IR.CheckTerm as IR
-- import qualified Juvix.Core.IR.Evaluator as Eval
import Juvix.Core.IR.Typechecker.Env as Env
import Juvix.Core.IR.Typechecker.Types as Typed
import qualified Juvix.Core.IR.Types as IR
import qualified Juvix.Core.IR.Types.Base as IR
-- import qualified Juvix.Core.Parameterisation as Param
import Juvix.Library hiding (Datatype)

-- import qualified Juvix.Library.Usage as Usage

typeCheckDeclaration ::
  [IR.RawDatatype' ext0 primTy0 primVal0] ->
  [IR.FunctionWith ty ext primTy primVal] ->
  IR.TypeCheck ty ext primTy primVal ()
typeCheckDeclaration [] [] =
  return undefined
typeCheckDeclaration ((IR.RawDatatype name lpos args levels cons) : tld) _ =
  undefined
-- TODO run checkDataType 0 [] [] p' dt
-- v <- eval [] dt
-- add to sig once typechecked
-- put $ addSig sig n (DataSig params pos sz v)
-- mapM_ (typeCheckConstructor n sz pos tel) cs
typeCheckDeclaration _ ((IR.Function name usage ty cls) : tlf) =
  undefined
-- TODO run typeCheckFuns
