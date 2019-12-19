{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -fplugin-opt=Foreign.Storable.Generic.Plugin:-v1 #-}
{-# OPTIONS_GHC -fplugin=Foreign.Storable.Generic.Plugin #-}

module Juvix.Backends.LLVM.JIT.Types where

import Foreign.Ptr (FunPtr, castFunPtr)
import Foreign.Storable.Generic
import qualified Juvix.INetIR.Types as IR
import Juvix.Library

foreign import ccall "dynamic" word32Fn ∷ FunPtr (Word32 → IO Word32) → (Word32 → IO Word32)

foreign import ccall "dynamic" doubleFn ∷ FunPtr (Double → IO Double) → (Double → IO Double)

foreign import ccall "dynamic" nodeFn ∷ FunPtr (Ptr Node → IO ()) → (Ptr Node → IO ())

type Port = IR.Port

type Node = IR.Node ()

instance Storable Port

-- TODO: Gstorable, need to alter library.

instance Storable Node

-- TODO: GStorable, need to alter library.

data OptimisationLevel
  = -- TODO: Determine if none / O0 are equivalent.
    None
  | O0
  | O1
  | O2
  | O3
  deriving (Show, Eq)

data Config
  = Config
      { configOptimisationLevel ∷ OptimisationLevel
      }
  deriving (Show, Eq)

class DynamicImport a where

  unFunPtr ∷ FunPtr a → a

  castImport ∷ ∀ b. FunPtr b → a
  castImport = unFunPtr . castFunPtr

instance DynamicImport (Word32 → IO Word32) where
  unFunPtr = word32Fn

instance DynamicImport (Double → IO Double) where
  unFunPtr = doubleFn

instance DynamicImport (Ptr Node → IO ()) where
  unFunPtr = nodeFn
