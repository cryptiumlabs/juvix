-- | Parameterization and application of the LLVM backend primitives.
module Juvix.Backends.LLVM.Parameterization
  ( llvm,
  )
where

import Juvix.Backends.LLVM.Primitive
import qualified Juvix.Core.Application as App
import qualified Juvix.Core.Parameterisation as Param
import Juvix.Library

instance Param.CanApply (PrimVal ext) where
  arity val = case val of
    App.Cont {} -> App.numLeft val
    App.Return {} -> arityRaw $ App.retTerm val

-- | Parameters for the LLVM backend.
llvm :: Param.Parameterisation PrimTy RawPrimVal
llvm =
  Param.Parameterisation
    { Param.hasType = undefined,
      Param.builtinTypes = undefined,
      Param.builtinValues = undefined,
      Param.stringVal = undefined,
      Param.intVal = undefined,
      Param.floatVal = undefined
    }
