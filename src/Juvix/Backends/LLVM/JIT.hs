module Juvix.Backends.LLVM.JIT
  ( module Juvix.Backends.LLVM.JIT.Execution,
    module Juvix.Backends.LLVM.JIT.Types,
    NetAPI (..),
    jitToNetAPI,
  )
where

import Foreign.Marshal.Array
import Foreign.Storable
import Juvix.Backends.LLVM.JIT.Execution
import Juvix.Backends.LLVM.JIT.Types
import Juvix.Library
import qualified LLVM.AST as AST

data NetAPI
  = NetAPI
      { createNet ∷ IO OpaqueNetPtr,
        appendToNet ∷ OpaqueNetPtr → [Node] → IO (),
        readNet ∷ OpaqueNetPtr → IO [Node],
        reduceUntilComplete ∷ OpaqueNetPtr → IO ()
      }

jitToNetAPI ∷ Config → AST.Module → IO (NetAPI, IO ())
jitToNetAPI config mod = do
  putText "Loading into LLVM runtime..."
  (imp, kill) ← mcJitWith config mod dynamicImport
  putText "Importing functions...."
  Just createNetFn ← importAs imp "createNet" (Proxy ∷ Proxy (IO OpaqueNetPtr)) (Proxy ∷ Proxy ()) (Proxy ∷ Proxy OpaqueNetPtr)
  Just appendToNetFn ← importAs imp "appendToNet" (Proxy ∷ Proxy (OpaqueNetPtr → Ptr Node → Int → IO ())) (Proxy ∷ Proxy (OpaqueNetPtr, Ptr Node, Int)) (Proxy ∷ Proxy ())
  Just readNetFn ← importAs imp "readNet" (Proxy ∷ Proxy (OpaqueNetPtr → IO (Ptr Nodes))) (Proxy ∷ Proxy OpaqueNetPtr) (Proxy ∷ Proxy (Ptr Nodes))
  Just reduceUntilCompleteFn ← importAs imp "reduceUntilComplete" (Proxy ∷ Proxy (OpaqueNetPtr → IO ())) (Proxy ∷ Proxy OpaqueNetPtr) (Proxy ∷ Proxy ())
  putText "Generating API..."
  pure
    ( NetAPI
        { createNet = createNetFn (),
          appendToNet = \ptr nodes → do
            let len = length nodes
            arr ← newArray nodes
            appendToNetFn (ptr, arr, len),
          readNet = \ptr → do
            (Nodes arr len) ← peek =<< readNetFn ptr
            peekArray len arr,
          reduceUntilComplete = reduceUntilCompleteFn
        },
      kill
    )
