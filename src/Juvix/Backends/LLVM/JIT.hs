module Juvix.Backends.LLVM.JIT
  ( module Juvix.Backends.LLVM.JIT.Execution,
    module Juvix.Backends.LLVM.JIT.Types,
    GraphAPI (..),
    jitToGraphAPI,
  )
where

import Juvix.Backends.LLVM.JIT.Execution
import Juvix.Backends.LLVM.JIT.Types
import Juvix.Library
import qualified LLVM.AST as AST

data GraphAPI
  = GraphAPI
      { createNet ∷ IO OpaqueNetPtr,
        appendToNet ∷ OpaqueNetPtr → Ptr Node → IO (),
        readNet ∷ OpaqueNetPtr → IO (Ptr Node),
        reduceUntilComplete ∷ OpaqueNetPtr → IO ()
      }

jitToGraphAPI ∷ Config → AST.Module → IO (GraphAPI, IO ())
jitToGraphAPI config mod = do
  (imp, kill) ← jitWith config mod dynamicImport
  Just createNetFn ← importAs imp "createNet" (Proxy ∷ Proxy (IO OpaqueNetPtr)) (Proxy ∷ Proxy ()) (Proxy ∷ Proxy OpaqueNetPtr)
  Just appendToNetFn ← importAs imp "appendToNet" (Proxy ∷ Proxy (OpaqueNetPtr → Ptr Node → IO ())) (Proxy ∷ Proxy (OpaqueNetPtr, Ptr Node)) (Proxy ∷ Proxy ())
  Just readNetFn ← importAs imp "readNet" (Proxy ∷ Proxy (OpaqueNetPtr → IO (Ptr Node))) (Proxy ∷ Proxy OpaqueNetPtr) (Proxy ∷ Proxy (Ptr Node))
  Just reduceUntilCompleteFn ← importAs imp "reduceUntilComplete" (Proxy ∷ Proxy (OpaqueNetPtr → IO ())) (Proxy ∷ Proxy OpaqueNetPtr) (Proxy ∷ Proxy ())
  pure
    ( GraphAPI
        { createNet = createNetFn (),
          appendToNet = curry appendToNetFn,
          readNet = readNetFn,
          reduceUntilComplete = reduceUntilCompleteFn
        },
      kill
    )
