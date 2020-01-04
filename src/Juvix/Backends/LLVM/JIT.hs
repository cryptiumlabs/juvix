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
  Just createNetFn ←
    importAs
      imp
      "create_net"
      (Proxy ∷ Proxy (IO OpaqueNetPtr))
      (Proxy ∷ Proxy ())
      (Proxy ∷ Proxy OpaqueNetPtr)
  Just appendToNetFn ←
    importAs
      imp
      "append_to_net"
      (Proxy ∷ Proxy (OpaqueNetPtr → Ptr Node → Int → IO ()))
      (Proxy ∷ Proxy (OpaqueNetPtr, Ptr Node, Int))
      (Proxy ∷ Proxy ())
  Just readNetFn ←
    importAs
      imp
      "read_net"
      (Proxy ∷ Proxy (OpaqueNetPtr → IO (Ptr Nodes)))
      (Proxy ∷ Proxy OpaqueNetPtr)
      (Proxy ∷ Proxy (Ptr Nodes))
  Just reduceUntilCompleteFn ←
    importAs
      imp
      "reduce_until_complete"
      (Proxy ∷ Proxy (OpaqueNetPtr → IO ()))
      (Proxy ∷ Proxy OpaqueNetPtr)
      (Proxy ∷ Proxy ())
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
