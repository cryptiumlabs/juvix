-- | Representation of LLVM primitives in Juvix.
module Juvix.Backends.LLVM.Primitive
  ( RawPrimVal (..),
    arityRaw,
  )
where

import Juvix.Library

-- | Raw representation of some primitives of LLVM.
data RawPrimVal
  = Add
  | Sub

-- | Arity of `RawPrimVal`.
arityRaw :: RawPrimVal -> Natural
arityRaw Add = 2
arityRaw Sub = 2
