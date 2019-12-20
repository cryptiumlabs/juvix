module Juvix.Backends.LLVM.JIT
  ( module Juvix.Backends.LLVM.JIT.Execution,
    module Juvix.Backends.LLVM.JIT.Types,
    NetAPI (..),
    jitToNetAPI,
  )
where

import Juvix.Backends.LLVM.JIT.Execution
import Juvix.Backends.LLVM.JIT.Types
import Juvix.Library
import qualified LLVM.AST as AST

-- TODO: These should be arrays (foreign array)
-- TODO: These should accept lists
data NetAPI
  = NetAPI
      { createNet ∷ IO OpaqueNetPtr,
        appendToNet ∷ OpaqueNetPtr → Ptr Node → IO (),
        readNet ∷ OpaqueNetPtr → IO (Ptr Node),
        reduceUntilComplete ∷ OpaqueNetPtr → IO ()
      }

jitToNetAPI ∷ Config → AST.Module → IO (NetAPI, IO ())
jitToNetAPI config mod = do
  (imp, kill) ← jitWith config mod dynamicImport
  Just createNetFn ← importAs imp "createNet" (Proxy ∷ Proxy (IO OpaqueNetPtr)) (Proxy ∷ Proxy ()) (Proxy ∷ Proxy OpaqueNetPtr)
  Just appendToNetFn ← importAs imp "appendToNet" (Proxy ∷ Proxy (OpaqueNetPtr → Ptr Node → IO ())) (Proxy ∷ Proxy (OpaqueNetPtr, Ptr Node)) (Proxy ∷ Proxy ())
  Just readNetFn ← importAs imp "readNet" (Proxy ∷ Proxy (OpaqueNetPtr → IO (Ptr Node))) (Proxy ∷ Proxy OpaqueNetPtr) (Proxy ∷ Proxy (Ptr Node))
  Just reduceUntilCompleteFn ← importAs imp "reduceUntilComplete" (Proxy ∷ Proxy (OpaqueNetPtr → IO ())) (Proxy ∷ Proxy OpaqueNetPtr) (Proxy ∷ Proxy ())
  pure
    ( NetAPI
        { createNet = createNetFn (),
          appendToNet = curry appendToNetFn,
          readNet = readNetFn,
          reduceUntilComplete = reduceUntilCompleteFn
        },
      kill
    )
