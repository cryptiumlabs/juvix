module Juvix.Backends.LLVM.Net.API where

-- TODO ∷ abstract all all imports to LLVM
import qualified Juvix.Backends.LLVM.Codegen as Codegen
import qualified Juvix.Backends.LLVM.Net.EAC as EAC
import qualified Juvix.Backends.LLVM.Net.EAC.Types as Types
import Juvix.Library hiding (reduce)
import qualified LLVM.AST.AddrSpace as Addr
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.IntegerPredicate as IntPred
import qualified LLVM.AST.Operand as Operand
import qualified LLVM.AST.Type as Type

-- TODO: c2hs / hs2c (whatever) for conversion types?
-- for now: manually

type OpaqueNet = Word32

int32 ∷ Type.Type
int32 = Codegen.int

nodeKind ∷ Type.Type
nodeKind = Codegen.i8

nodeAddress ∷ Type.Type
nodeAddress = Codegen.int

nodeAddressPointer ∷ Type.Type
nodeAddressPointer = Type.PointerType nodeAddress (Addr.AddrSpace 0)

nodeType ∷ Type.Type
nodeType = Type.NamedTypeReference "node"

eacListPointer ∷ Type.Type
eacListPointer = Types.eacLPointer

nodePointer ∷ Type.Type
nodePointer = Type.PointerType nodeType (Addr.AddrSpace 0)

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
opaqueNetType = Type.PointerType eacListPointer (Addr.AddrSpace 0)

-- This API model passes pointers back & forth.
-- createNet :: IO (Ptr Net)
-- appendToNet :: Ptr Net -> [Node] -> IO ()
-- readNet :: Ptr Net -> IO [Node]
-- reduceUntilComplete :: Ptr Net -> IO ()
-- test :: IO ()

defineTest ∷ Codegen.Define m ⇒ m Operand.Operand
defineTest =
  Codegen.defineFunction Type.void "test" [] $ do
    create_net ← Codegen.externf "create_net"
    append_to_net ← Codegen.externf "append_to_net"
    reduce_until_complete ← Codegen.externf "reduce_until_complete"
    ptr ← Codegen.call opaqueNetType create_net (Codegen.emptyArgs [])
    Codegen.callVoid
      append_to_net
      ( Codegen.emptyArgs
          [ ptr,
            Operand.ConstantOperand (C.Null nodePointer),
            Operand.ConstantOperand (C.Int Codegen.addressSpace 0)
          ]
      )
    Codegen.callVoid reduce_until_complete (Codegen.emptyArgs [ptr])
    Codegen.retNull

defineCreateNet ∷ Codegen.Define m ⇒ m Operand.Operand
defineCreateNet =
  Codegen.defineFunction opaqueNetType "create_net" [] $ do
    -- Note: this is not a pointer to an EAC list, but rather a pointer to a pointer to an EAC list.
    -- This is intentional since we need to malloc when `appendToNet` is called.
    eac ← Codegen.malloc Codegen.addressSpace opaqueNetType
    -- Just return the pointer.
    Codegen.ret eac

defineReadNet ∷ Codegen.Define m ⇒ m Operand.Operand
defineReadNet =
  Codegen.defineFunction nodePointer "read_net" [(opaqueNetType, "net")] $ do
    netPtr ← Codegen.externf "net"
    _net ← Codegen.load eacListPointer netPtr
    -- TODO: Walk the current net, return a list of nodes
    -- Can we do this? Need top node ptr & traversal.
    -- Maybe return duplicate nodes & de-duplicate in haskell.
    ret ← Codegen.malloc Codegen.addressSpace nodePointer
    Codegen.ret ret

defineAppendToNet ∷
  (Codegen.Debug m, Codegen.Define m, Codegen.MallocNode m) ⇒ m Operand.Operand
defineAppendToNet =
  Codegen.defineFunction Type.void "append_to_net" args $ do
    netPtr ← Codegen.externf "net"
    topNode ← EAC.mallocTop
    appNode ← EAC.mallocApp
    lamNode ← EAC.mallocLam
    lamNode2 ← EAC.mallocLam
    aux1 ← Codegen.auxiliary1
    aux2 ← Codegen.auxiliary2
    main ← Codegen.mainPort
    Codegen.link [topNode, aux1, appNode, aux1]
    Codegen.link [appNode, main, lamNode, main]
    Codegen.link [appNode, aux2, lamNode2, main]
    Codegen.link [lamNode, aux1, lamNode, aux2]
    Codegen.link [lamNode2, aux1, lamNode2, aux2]
    eac_list ← Types.cons appNode (Operand.ConstantOperand (C.Null Types.eacLPointer))
    Codegen.store netPtr eac_list
    Codegen.retNull
  where
    args = [(opaqueNetType, "net"), (nodePointer, "nodes"), (int32, "node_count")]

defineAppendToNet' ∷
  (Codegen.Debug m, Codegen.Define m, Codegen.MallocNode m) ⇒ m Operand.Operand
defineAppendToNet' =
  Codegen.defineFunction Type.void "append_to_net" args $ do
    nodes ← Codegen.externf "nodes"
    node_count ← Codegen.externf "node_count"
    forLoop ← Codegen.addBlock "for.loop"
    forExit ← Codegen.addBlock "for.exit"
    forLoop2 ← Codegen.addBlock "for.loop.2"
    forExit2 ← Codegen.addBlock "for.exit.2"
    -- Create a counter to track position
    counter ← Codegen.alloca int32
    Codegen.store counter (Operand.ConstantOperand (C.Int 32 0))
    _ ← Codegen.br forLoop
    -- Loop case: convert node, increment counter.
    Codegen.setBlock forLoop
    ind ← Codegen.load int32 counter
    -- Load node at index `ind`.
    _node ←
      Codegen.loadElementPtr
        ( Codegen.Minimal
            { Codegen.address' = nodes,
              Codegen.type' = nodePointer,
              Codegen.indincies' = [ind]
            }
        )
    -- Create the in-memory node.
    kind ← EAC.mallocApp -- TODO: Switch on node kind.
      -- Write the address to the list.
    addr ←
      Codegen.getElementPtr
        ( Codegen.Minimal
            { Codegen.address' = nodes,
              Codegen.type' = nodePointer,
              Codegen.indincies' = [ind, Operand.ConstantOperand (C.Int 32 0)]
            }
        )
    Codegen.store addr kind
    next ← Codegen.add int32 ind (Operand.ConstantOperand (C.Int 32 1))
    Codegen.store counter next
    cond ← Codegen.icmp IntPred.EQ node_count next
    _ ← Codegen.cbr cond forLoop forExit
    -- Exit case: next loop.
    Codegen.setBlock forExit
    _ ← Codegen.retNull
    Codegen.store counter (Operand.ConstantOperand (C.Int 32 0))
    _ ← Codegen.br forLoop2
    -- Second loop: link nodes.
    Codegen.setBlock forLoop2
    ind ← Codegen.load int32 counter
    -- Load node at index `ind`.
    _node ←
      Codegen.loadElementPtr
        ( Codegen.Minimal
            { Codegen.address' = nodes,
              Codegen.type' = nodePointer,
              Codegen.indincies' = [ind]
            }
        )
    _ptr ←
      Codegen.loadElementPtr
        ( Codegen.Minimal
            { Codegen.address' = nodes,
              Codegen.type' = nodePointer,
              Codegen.indincies' = [ind, Operand.ConstantOperand (C.Int 32 0)]
            }
        )
    -- TODO: Link things, lookup node pointers.
    -- Alter parameter?
    next ← Codegen.add int32 ind (Operand.ConstantOperand (C.Int 32 1))
    Codegen.store counter next
    cond ← Codegen.icmp IntPred.EQ node_count next
    _ ← Codegen.cbr cond forLoop2 forExit2
    -- Exit case: return.
    Codegen.setBlock forExit2
    -- TODO: Set eac list pointer?
    Codegen.retNull
  where
    args = [(opaqueNetType, "net"), (nodePointer, "nodes"), (int32, "node_count")]

defineReduceUntilComplete ∷ Codegen.Define m ⇒ m Operand.Operand
defineReduceUntilComplete =
  Codegen.defineFunction Type.void "reduce_until_complete" [(opaqueNetType, "net")] $ do
    -- Load the current EAC list pointer.
    netPtr ← Codegen.externf "net"
    net ← Codegen.load eacListPointer netPtr
    -- Call reduce, which recurses until there are no primary pairs left.
    reduce ← Codegen.externf "reduce"
    _ ← Codegen.callVoid reduce (Codegen.emptyArgs [net])
    -- Return.
    Codegen.retNull
