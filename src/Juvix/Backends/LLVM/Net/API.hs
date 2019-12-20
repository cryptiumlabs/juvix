module Juvix.Backends.LLVM.Net.API where

-- TODO ∷ abstract all all imports to LLVM
import qualified Juvix.Backends.LLVM.Codegen as Codegen
import Juvix.Library hiding (reduce)
import qualified Juvix.Backends.LLVM.Net.EAC.Types as Types
import qualified LLVM.AST.AddrSpace as Addr
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.IntegerPredicate as IntPred
import qualified LLVM.AST.Name as Name
import qualified LLVM.AST.Operand as Operand
import qualified LLVM.AST.Type as Type

-- TODO: c2hs / hs2c (whatever) for conversion types?
-- for now: manually

type OpaqueNet = Word32

word32 ∷ Type.Type
word32 = Codegen.int

nodeKind ∷ Type.Type
nodeKind = Codegen.i4

nodeAddress ∷ Type.Type
nodeAddress = Codegen.int

nodeAddressPointer ∷ Type.Type
nodeAddressPointer = Type.PointerType nodeAddress (Addr.AddrSpace 32)

nodeType ∷ Type.Type
nodeType = Type.NamedTypeReference "node"

nodePointer ∷ Type.Type
nodePointer = Type.PointerType nodeType (Addr.AddrSpace 32)

node ∷ Type.Type
node = Type.StructureType
  { Type.isPacked = True,
    Type.elementTypes =
      [ nodeAddress,
        nodeKind,
        nodeAddressPointer
      ]
  }

opaqueNetType ∷ Type.Type
opaqueNetType = Type.PointerType Types.eacLPointer (Addr.AddrSpace 32)

-- This API model passes pointers back & forth.
-- createNet :: IO (Ptr Net)
-- appendToNet :: Ptr Net -> [Node] -> IO ()
-- readNet :: Ptr Net -> IO [Node]
-- reduceUntilComplete :: Ptr Net -> IO ()

defineCreateNet ∷ Codegen.Define m ⇒ m Operand.Operand
defineCreateNet =
  Codegen.defineFunction opaqueNetType "createNet" [] $ do
    -- Note: this is not a pointer to an EAC list, but rather a pointer to a pointer to an EAC list.
    -- This is intentional since we need to malloc when `appendToNet` is called.
    eac ← Codegen.malloc 32 Types.eacLPointer
    -- Just return the pointer.
    Codegen.ret eac

defineReadNet ∷ Codegen.Define m ⇒ m Operand.Operand
defineReadNet =
  Codegen.defineFunction Type.void "readNet" [(opaqueNetType, "net")] $ do
    netPtr ← Codegen.externf "net"
    net ← Codegen.load Types.eacLPointer netPtr
    -- TODO: Walk the current net, return a list of nodes
    -- Can we do this? Need top node ptr & traversal.
    Codegen.retNull

defineAppendToNet ∷ Codegen.Define m ⇒ m Operand.Operand
defineAppendToNet =
  Codegen.defineFunction Type.void "appendToNet" [(nodePointer, "nodes"), (word32, "node_count")] $ do
    -- Create a counter to track position
    counter ← Codegen.alloca nodeType
    -- TODO
    -- Append nodes to the net
    -- Call `createNode` on each
    -- Append to the primary pair list as necessary
    Codegen.retNull

defineReduceUntilComplete ∷ Codegen.Define m ⇒ m Operand.Operand
defineReduceUntilComplete =
  Codegen.defineFunction Type.void "reduceUntilComplete" [(opaqueNetType, "net")] $ do
    -- Load the current EAC list pointer.
    netPtr ← Codegen.externf "net"
    net ← Codegen.load Types.eacLPointer netPtr
    -- Call reduce, which recurses until there are no primary pairs left.
    Codegen.callGen Type.void [net] "reduce"
    -- Return.
    Codegen.retNull
