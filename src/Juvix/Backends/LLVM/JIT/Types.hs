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

foreign import ccall "dynamic" createNetFn ∷ FunPtr (IO OpaqueNetPtr) → IO OpaqueNetPtr

foreign import ccall "dynamic" appendToNetFn ∷ FunPtr (OpaqueNetPtr → Ptr Node → Int → IO ()) → (OpaqueNetPtr → Ptr Node → Int → IO ())

foreign import ccall "dynamic" readNetFn ∷ FunPtr (OpaqueNetPtr → IO (Ptr Nodes)) → (OpaqueNetPtr → IO (Ptr Nodes))

foreign import ccall "dynamic" reduceUntilCompleteFn ∷ FunPtr (OpaqueNetPtr → IO ()) → (OpaqueNetPtr → IO ())

type OpaqueNetPtr = Ptr Word32

type Port = IR.Port

type Kind = IR.Kind ()

type Node = IR.Node ()

data Nodes
  = Nodes
      { nodeArray ∷ Ptr Node,
        nodeCount ∷ Int
      }
  deriving (Generic)

instance GStorable Nodes

instance GStorable Port

-- TODO: Gstorable, need to alter library.

instance GStorable Node

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

class DynamicImport a b | b → a where

  unFunPtr ∷ FunPtr a → b

  castImport ∷ ∀ c. FunPtr c → b
  castImport = (unFunPtr ∷ FunPtr a → b) . castFunPtr

instance DynamicImport (Word32 → IO Word32) (Word32 → IO Word32) where
  unFunPtr = word32Fn

instance DynamicImport (Double → IO Double) (Double → IO Double) where
  unFunPtr = doubleFn

instance DynamicImport (Ptr Node → IO ()) (Ptr Node → IO ()) where
  unFunPtr = nodeFn

instance DynamicImport (IO OpaqueNetPtr) (() → IO OpaqueNetPtr) where
  unFunPtr = const . createNetFn

instance DynamicImport (OpaqueNetPtr → Ptr Node → Int → IO ()) ((OpaqueNetPtr, Ptr Node, Int) → IO ()) where
  unFunPtr = uncurry3 . appendToNetFn

instance DynamicImport (OpaqueNetPtr → IO (Ptr Nodes)) (OpaqueNetPtr → IO (Ptr Nodes)) where
  unFunPtr = readNetFn

instance DynamicImport (OpaqueNetPtr → IO ()) (OpaqueNetPtr → IO ()) where
  unFunPtr = reduceUntilCompleteFn
