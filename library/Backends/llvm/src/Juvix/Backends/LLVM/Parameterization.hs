-- | Parameterization and application of the LLVM backend primitives.
module Juvix.Backends.LLVM.Parameterization
  (
  )
where

import Juvix.Backends.LLVM.Primitive
import qualified Juvix.Core.Application as App
import qualified Juvix.Core.Parameterisation as Param

instance Param.CanApply (PrimVal ext) where
  arity val = case val of
    App.Cont {} -> App.numLeft val
    App.Return {} -> arityRaw $ App.retTerm val
