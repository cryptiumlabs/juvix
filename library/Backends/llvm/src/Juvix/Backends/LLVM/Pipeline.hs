-- | The basic connection between the backend and the Juvix pipeline.
module Juvix.Backends.LLVM.Pipeline
  ( BLLVM (..),
  )
where

import Juvix.Backends.LLVM.Primitive
import Juvix.Library
import qualified Juvix.Pipeline as Pipeline

-- | Identifier for the LLVM backend.
data BLLVM = BLLVM
  deriving (Show, Eq)

instance Pipeline.HasBackend BLLVM where
  type Ty BLLVM = PrimTy
  type Val BLLVM = RawPrimVal
