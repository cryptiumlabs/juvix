-- | The basic connection between the backend and the Juvix pipeline.
module Juvix.Backends.LLVM.Pipeline
  ( BLLVM (..),
  )
where

import qualified Data.HashMap.Strict as HM
import Juvix.Backends.LLVM.Compilation
import Juvix.Backends.LLVM.Parameterization
import Juvix.Backends.LLVM.Primitive
import qualified Juvix.Core.Erased.Ann as ErasedAnn
import qualified Juvix.Core.IR as IR
import Juvix.Library
import Juvix.Library.Feedback
import qualified Juvix.Pipeline as Pipeline
import qualified Juvix.ToCore.FromFrontend as FF

-- | Identifier for the LLVM backend.
data BLLVM = BLLVM
  deriving (Show, Eq)

instance Pipeline.HasBackend BLLVM where
  type Ty BLLVM = PrimTy
  type Val BLLVM = RawPrimVal
  type Err BLLVM = CompilationError

  stdlibs _ = ["stdlib/LLVM.ju"]

  -- Copied over from the Michelson backend, and adapter where necessary.
  typecheck ctx = Pipeline.typecheck' ctx llvm Set

  compile out term = do
    let raw = ErasedAnn.toRaw term
    code <- compileProgram raw
    Pipeline.writeout out code
