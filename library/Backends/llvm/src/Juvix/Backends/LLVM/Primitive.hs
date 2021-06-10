-- | Representation of LLVM primitives in Juvix.
module Juvix.Backends.LLVM.Primitive
  ( PrimTy (..),
    arityTy,
    RawPrimVal (..),
    PrimVal,
    CompilationError (..),
    arityRaw,
  )
where

import qualified Juvix.Core.Application as App
import qualified Juvix.Core.Parameterisation as Param
import Juvix.Library
import qualified LLVM.AST.Type as LLVM

-- | Intermediate representation of types.
data PrimTy
  = -- | TODO: Rely on the LLVM-defined types for now.
    PrimTy LLVM.Type
  | Set
  deriving (Eq, Show, Read)

-- | TODO: A placeholder arity implementation for types.
arityTy :: PrimTy -> Natural
arityTy _ = 0

-- | Raw representation of some primitives of LLVM.
data RawPrimVal
  = Add
  | Sub
  | LitInt Integer
  deriving (Eq, Show, Read)

-- | The primitive values as exposed to users of Juvix, wrapping inside a
-- return or a continuation.
type PrimVal ext = App.Return' ext (Param.PrimType PrimTy) RawPrimVal

-- | Custom compilation errors.
data CompilationError
  = -- | TODO: Just a placeholder for now.
    PlaceHolderError
  deriving (Eq, Show)

-- | Arity of `RawPrimVal`.
arityRaw :: RawPrimVal -> Natural
arityRaw Add = 2
arityRaw Sub = 2
