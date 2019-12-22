module Juvix.Backends.LLVM.Net.API where

-- TODO ∷ abstract all all imports to LLVM
import qualified Juvix.Backends.LLVM.Codegen as Codegen
import qualified Juvix.Backends.LLVM.Net.EAC as EAC
import qualified Juvix.Backends.LLVM.Net.EAC.Types as Types
import Juvix.Library hiding (reduce)
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

eacListPointer ∷ Type.Type
eacListPointer = Type.PointerType (Type.NamedTypeReference "list") (Addr.AddrSpace 32)

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
opaqueNetType = Type.PointerType eacListPointer (Addr.AddrSpace 32)

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
    eac ← Codegen.malloc 32 eacListPointer
    -- Just return the pointer.
    Codegen.ret eac

defineReadNet ∷ Codegen.Define m ⇒ m Operand.Operand
defineReadNet =
  Codegen.defineFunction Type.void "readNet" [(opaqueNetType, "net")] $ do
    netPtr ← Codegen.externf "net"
    net ← Codegen.load eacListPointer netPtr
    -- TODO: Walk the current net, return a list of nodes
    -- Can we do this? Need top node ptr & traversal.
    -- Maybe return duplicate nodes & de-duplicate in haskell.
    Codegen.retNull

defineAppendToNet ∷ (Codegen.Define m, Codegen.MallocNode m) ⇒ m Operand.Operand
defineAppendToNet =
  Codegen.defineFunction Type.void "appendToNet" [(nodePointer, "nodes"), (word32, "node_count")] $ do
    nodes ← Codegen.externf "nodes"
    node_count ← Codegen.externf "node_count"
    forLoop ← Codegen.addBlock "for.loop"
    forExit ← Codegen.addBlock "for.exit"
    -- Create a counter to track position
    counter ← Codegen.alloca word32
    Codegen.store counter (Operand.ConstantOperand (C.Int 32 0))
    Codegen.br forLoop
    -- Loop case: convert node, increment counter.
    Codegen.setBlock forLoop
    ind ← Codegen.load word32 counter
    -- Load node at index `ind`.
    nodePtr ←
      Codegen.getElementPtr
        ( Codegen.Minimal
            { Codegen.address' = nodes,
              Codegen.type' = nodePointer,
              Codegen.indincies' = [Operand.ConstantOperand (C.Int 32 0), ind]
            }
        )
    node ← Codegen.load node nodePtr
    -- Create the in-memory node.
    kind ← EAC.mallocApp -- TODO: Switch on node kind.
      -- TODO: Link things (second loop iteration? need the ability to look up node pointers)
    next ← Codegen.add word32 ind (Operand.ConstantOperand (C.Int 32 0))
    Codegen.store counter next
    cond ← Codegen.icmp IntPred.EQ node_count counter
    Codegen.cbr cond forLoop forExit
    -- Exit case: return.
    Codegen.setBlock forExit
    -- TODO: Primary pairs?
    Codegen.retNull

defineReduceUntilComplete ∷ Codegen.Define m ⇒ m Operand.Operand
defineReduceUntilComplete =
  Codegen.defineFunction Type.void "reduceUntilComplete" [(opaqueNetType, "net")] $ do
    -- Load the current EAC list pointer.
    netPtr ← Codegen.externf "net"
    net ← Codegen.load eacListPointer netPtr
    -- Call reduce, which recurses until there are no primary pairs left.
    Codegen.callGen Type.void [net] "reduce"
    -- Return.
    Codegen.retNull
