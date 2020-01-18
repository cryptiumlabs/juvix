-- | Operations necessary to update nodes
-- - =mainPort=, =auxiliary1= \dots =auxiliary4= allocation
--   | Part       | Alloca Or Malloc |
--   |------------+------------------|
--   | MainPort   | Malloc top level |
--   | Auxiliary1 | Malloc top level |
--   | Auxiliary2 | Malloc top level |
--   | Auxiliary3 | Malloc top level |
--   | Auxiliary4 | Malloc top level |
--
-- - =mallocNodeH= Allocation
--
--   + layout :
--     Node[portSize | PortArray[portLocation | NodePtr] | DataArray[Data]]
--
--   | Part         | Alloca Or Malloc                   |
--   |--------------+------------------------------------|
--   | Node         | Malloc                             |
--   | portSize     | Stored on Node malloc              |
--   | PortArray    | Malloc                             |
--   | DataArray    | Malloc Maybe                       |
--   | PortLocation | (Null) Allocad from PortArray Call |
--   | NodePtr      | (Null) Allocad from PortArray Call |
--   | Data         | (Null) Allocad from DataArray Call |
--
--   + _Sub allocation functions used_
--
--     * =mallocNode=
--       | node | Malloc |
--
--     * =allocaNumPortNum=
--       | portsSize | Alloca |
--
--     * =mallocPortsH=
--       | portArray | Malloc |
--
--     * =mallocDataH=
--       | dataArray | Malloc |
--
--   + the values that are null will be set from outside when the node
--     is instantiated.
--     * Data will be **Allocad**
--
--     * Port Location is shown to be **malloc** above by =mainPort=
--       \dots =Auxiliary4=. However in the future we may **alloca** a value
--       to store here
--
--     * NodePtr is **mallocd** in the same way this node is, and thus
--       is external
--
--
-- - Notably PortLocation, NodePtr, and Data are not allocated here,
--   but are instead sent in.
--
-- - Currently =defineMainPort=, =defineAuxiliary1= \dots
--   =defineAuxiliary4= malloc the first four ports, and this is what
--   link sets for the nodes.
--
--   + This has some trade offs, namely we don't have to alloca more
--     ports, however this will lead to waste if say =auxiliary4= is
--     never used.
--
--   + In the future this should turn to an alloca, and thus to
--     dealloc a node, we need not iterate over i.
--
-- - For deallocation, just deallocate the node pointer itself
--
--   + Currently, node pointers are allocated when nodes are made, and
--     so are not the responsibility of a node to deallocate all the
--     pointers.
--     * this however is up to the Net representation themselves, and
--       thus should modify the default deallocate node functionality
module Juvix.Backends.LLVM.Codegen.Graph.Definitions where

import qualified Juvix.Backends.LLVM.Codegen.Block as Block
import qualified Juvix.Backends.LLVM.Codegen.Graph.Debug as Debug
import qualified Juvix.Backends.LLVM.Codegen.Graph.Operations as Ops
import Juvix.Backends.LLVM.Codegen.Types as Types
import Juvix.Library hiding (Type, link, local)
import LLVM.AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Global as Global
import qualified LLVM.AST.IntegerPredicate as IntPred
import qualified LLVM.AST.Name as Name
import qualified LLVM.AST.Operand as Operand
import qualified LLVM.AST.Type as Type

--------------------------------------------------------------------------------
-- Call Alias for Main Functions
--------------------------------------------------------------------------------

callGen ∷
  Types.Call m ⇒ Type.Type → [Operand.Operand] → Name.Name → m Operand.Operand
callGen typ' args fn = do
  f ← Block.externf fn
  Block.call typ' f (Block.emptyArgs args)

callGenVoid ∷
  Types.Call m ⇒ [Operand.Operand] → Name.Name → m ()
callGenVoid args fn = do
  f ← Block.externf fn
  Block.callVoid f (Block.emptyArgs args)

link,
  linkConnectedPort,
  rewire ∷
    Call m ⇒ [Operand.Operand] → m ()
link args = callGenVoid args "link"
rewire args = callGenVoid args "rewire"
linkConnectedPort args = callGenVoid args "link_connected_port"

isBothPrimary,
  findEdge ∷
    Call m ⇒ [Operand.Operand] → m Operand.Operand
isBothPrimary args = callGen (Types.pointerOf Types.bothPrimary) args "is_both_primary"
findEdge args = callGen Types.portPointer args "find_edge"

--------------------------------------------------------------------------------
-- Main Functions
--------------------------------------------------------------------------------

-- TODO ∷ delNodes, deleteRewire, deleteEdge, allocaNode in full

-- TODO ∷ abstract over the define pattern seen below?

defineLink ∷ (Define m, Debug m) ⇒ m Operand.Operand
defineLink = Block.defineFunction Type.void "link" args $
  do
    aux1 ← auxiliary1
    aux2 ← auxiliary2
    node1 ← Block.externf "node_1"
    node2 ← Block.externf "node_1"
    Types.debugLevelOne $ do
      _ ← Block.printCString "Executing Link rule \n" []
      _ ← Block.printCString "Calling Link on \n" []
      Debug.printNodePort node1 aux1
      Debug.printNodePort node2 aux2
    Ops.setPort ("node_1", "port_1") ("node_2", "port_2")
    Types.debugLevelOne $ do
      _ ← Block.printCString "Calling Link on \n" []
      Debug.printNodePort node1 aux2
      Debug.printNodePort node2 aux1
    Ops.setPort ("node_2", "port_2") ("node_1", "port_1")
    Block.retNull
  where
    args =
      [ (nodePointer, "node_1"),
        (numPortsPointer, "port_1"),
        (nodePointer, "node_2"),
        (numPortsPointer, "port_2")
      ]

-- perform offsets

defineIsBothPrimary ∷ (Define m, Debug m) ⇒ m Operand.Operand
defineIsBothPrimary =
  Block.defineFunction (Types.pointerOf Types.bothPrimary) "is_both_primary" args $
    do
      Types.debugLevelOne $
        Block.printCString "Executing is_both_primary rule \n" [] >> pure ()
      return' ← Block.alloca Types.bothPrimary
      -- TODO ∷ should this call be abstracted somewhere?!
      mainPort ← mainPort
      nodePtr ← Block.externf "node_ptr"
      portPtr ← findEdge [nodePtr, mainPort]
      otherNodePtr ← Block.loadElementPtr $
        Types.Minimal
          { Types.type' = Types.nodePointer,
            Types.address' = portPtr,
            Types.indincies' = Block.constant32List [0, 0]
          }
      -- convert ptrs to ints
      nodeInt ← Block.ptrToInt nodePtr pointerSize
      otherNodeInt ← Block.ptrToInt otherNodePtr pointerSize
      -- compare the pointers to see if they are the same
      cmp ← Block.icmp IntPred.EQ nodeInt otherNodeInt
      tag ← Ops.getIsPrimaryEle return'
      nod ← Ops.getPrimaryNode Types.nodePointer return'
      Block.store tag cmp
      Block.store nod otherNodePtr
      Block.ret return'
  where
    args = [(Types.nodePointer, "node_ptr")]

-- The logic assumes that the operation always succeeds
defineFindEdge ∷ (Debug m, Define m) ⇒ m Operand.Operand
defineFindEdge =
  Block.defineFunction Types.portPointer "find_edge" args $
    do
      node ← Block.externf "node"
      pNum ← Block.externf "port"
      -----------------------------------------------------------------
      Types.debugLevelOne $ do
        _ ← Block.printCString "Executing find_edge rule \n" []
        _ ← Block.printCString "Finding node and edge of %p \n" [node]
        Debug.printNodePort node pNum
        pure ()
      -----------------------------------------------------------------
      portPtr ← Ops.getPort node pNum
      otherPortPtr ← Ops.portPointsTo portPtr
      Block.ret otherPortPtr
  where
    args = [(nodePointer, "node"), (numPortsPointer, "port")]

mallocNode ∷ Call m ⇒ Integer → m Operand.Operand
mallocNode size = Block.malloc size Types.nodePointer

-- H variants below mean that we are able to allocate from Haskell and
-- need not make a function

-- TODO ∷ could be storing data wrong... find out
mallocNodeH ∷
  ( RetInstruction m,
    Debug m,
    HasState "typTab" TypeTable m,
    HasState "varTab" VariantToType m,
    HasState "symTab" SymbolTable m
  ) ⇒
  [Maybe Operand.Operand] →
  [Maybe Operand.Operand] →
  Integer →
  m Operand.Operand
mallocNodeH mPorts mData extraData = do
  let anyThere xs =
        case length xs of
          -- 1 until it's safe to not to
          0 → 1
          _ → 1
      tagSize
        | Types.bitSizeEncodingPoint = 0
        | otherwise = Types.numPortsSize
  let totalSize =
        tagSize
          + (anyThere mPorts * Types.pointerSizeInt)
          + (anyThere mData * Types.pointerSizeInt)
          + extraData
  -- TODO ∷ see issue #262 on how to optimize out the loads and extra allocas
  nodePtr ← mallocNode (fromIntegral totalSize)
  -----------------------------------------------------------------
  Types.debugLevelOne $
    Block.printCString "Allocating node %p \n" [nodePtr] >> pure ()
  -- the bitCast is for turning the size of the array to 0
  -- for proper dynamically sized arrays
  ports ← mallocPortsH mPorts >>= flip Block.bitCast Types.portData
  portPtr ← Ops.getPortData nodePtr
  Block.store portPtr ports
  unless Types.bitSizeEncodingPoint $
    do
      portSizeP ← Ops.allocaNumPortNum (fromIntegral $ length mPorts)
      tagPtr ← Block.getElementPtr $
        Types.Minimal
          { Types.type' = Types.numPortsPointer,
            Types.address' = nodePtr,
            Types.indincies' = Block.constant32List [0, 0]
          }
      portSize ← Block.load numPortsNameRef portSizeP
      Block.store tagPtr portSize
  -- let's not allocate data if we don't have to!
  case anyThere mData ∷ Integer of
    -- do this when we pass more information to deAllocateNode
    -- Until then it's not safe
    -- 0 → pure ()
    _ → do
      data' ← mallocDataH mData >>= flip Block.bitCast Types.dataArray
      dataPtr ← Ops.getDataArray nodePtr
      Block.store dataPtr data'
  pure nodePtr

-- | used to create the malloc and alloca functions for ports and data
createGenH ∷
  RetInstruction m ⇒
  [Maybe Operand.Operand] →
  Type.Type →
  (Type.Type → Integer → m Operand.Operand) →
  m Operand.Operand
createGenH mPortData type' alloc = do
  ports ← alloc (Type.ArrayType len type') (fromIntegral len)
  traverse_
    ( \(p, i) →
        case p of
          Nothing → pure ()
          Just p → do
            ptr ← Block.getElementPtr $
              Types.Minimal
                { Types.type' = type',
                  Types.address' = ports,
                  Types.indincies' = Block.constant32List [0, i]
                }
            Block.store ptr p
    )
    (zip mPortData [0 ..])
  pure ports
  where
    len = fromIntegral (length mPortData ∷ Int)

mallocGenH ∷
  Call m ⇒ [Maybe Operand.Operand] → Type.Type → Integer → m Operand.Operand
mallocGenH mPortData type' dataSize =
  createGenH mPortData type' (\t len → Block.malloc (len * dataSize) (Types.pointerOf t))

allocaGenH ∷ RetInstruction m ⇒ [Maybe Operand.Operand] → Type.Type → m Operand.Operand
allocaGenH mPortData type' = createGenH mPortData type' (const . Block.alloca)

mallocPortsH ∷ Call m ⇒ [Maybe Operand.Operand] → m Operand.Operand
mallocPortsH mPorts =
  mallocGenH mPorts Types.portTypeNameRef Types.portTypeSize

allocaPortsH ∷ RetInstruction m ⇒ [Maybe Operand.Operand] → m Operand.Operand
allocaPortsH mPorts = allocaGenH mPorts Types.portTypeNameRef

allocaDataH ∷ RetInstruction m ⇒ [Maybe Operand.Operand] → m Operand.Operand
allocaDataH mPorts = allocaGenH mPorts Types.dataType

mallocDataH ∷ Call m ⇒ [Maybe Operand.Operand] → m Operand.Operand
mallocDataH mPorts =
  mallocGenH mPorts Types.dataType Types.dataTypeSize

-- derived from the core functions

defineLinkConnectedPort ∷ Define m ⇒ m Operand.Operand
defineLinkConnectedPort =
  Block.defineFunction Type.void "link_connected_port" args $
    do
      -- TODO ∷ Abstract out this bit ---------------------------------------------
      (nOld, pOld) ← (,) <$> Block.externf "node_old" <*> Block.externf "port_old"
      (nNew, pNew) ← (,) <$> Block.externf "node_new" <*> Block.externf "port_new"
      oldPointsTo ← findEdge [nOld, pOld] -- portPtr
      let intoGen typ num = Block.getElementPtr $
            Types.Minimal
              { Types.type' = typ,
                Types.address' = oldPointsTo,
                Types.indincies' = Block.constant32List [0, num]
              }
      numPointsTo ← intoGen numPortsPointer 1
      nodePointsToPtr ← intoGen (Types.pointerOf nodePointer) 0 >>= Block.load nodePointer
      -- End Abstracting out bits -------------------------------------------------
      _ ← link [nNew, pNew, nodePointsToPtr, numPointsTo]
      Block.retNull
  where
    args =
      [ (nodePointer, "node_old"),
        (numPortsPointer, "port_old"),
        (nodePointer, "node_new"),
        (numPortsPointer, "port_new")
      ]

defineRewire ∷ Define m ⇒ m Operand.Operand
defineRewire =
  Block.defineFunction Type.void "rewire" args $
    do
      -- TODO ∷ Abstract out this bit ---------------------------------------------
      (n1, p1) ← (,) <$> Block.externf "node_one" <*> Block.externf "port_one"
      (n2, p2) ← (,) <$> Block.externf "node_two" <*> Block.externf "port_two"
      oldPointsTo ← findEdge [n1, p1] -- portPtr
      let intoGen typ num = Block.getElementPtr $
            Types.Minimal
              { Types.type' = typ,
                Types.address' = oldPointsTo,
                Types.indincies' = Block.constant32List [0, num]
              }
      numPointsTo ← intoGen numPortsPointer 1
      nodePointsToPtr ← intoGen (Types.pointerOf nodePointer) 0 >>= Block.load nodePointer
      -- End Abstracting out bits -------------------------------------------------
      _ ← linkConnectedPort [n2, p2, nodePointsToPtr, numPointsTo]
      Block.retNull
  where
    args =
      [ (nodePointer, "node_one"),
        (numPortsPointer, "port_one"),
        (nodePointer, "node_two"),
        (numPortsPointer, "port_two")
      ]

deAllocateNode ∷ (Debug m, Define m) ⇒ Operand.Operand → m ()
deAllocateNode nodePtr = do
  Types.debugLevelOne $
    Block.printCString "De-allocating node %p \n" [nodePtr] >> pure ()
  portPtr ← Ops.loadPortData nodePtr
  dataPtr ← Ops.loadDataArray nodePtr
  _ ← Block.free portPtr
  _ ← Block.free dataPtr
  Block.free nodePtr

--------------------------------------------------------------------------------
-- Port Aliases
--------------------------------------------------------------------------------

-- TODO ∷ overload ports, so these are just ints and not a box around them

defineMainPort,
  defineAuxiliary1,
  defineAuxiliary2,
  defineAuxiliary3,
  defineAuxiliary4 ∷
    ( HasState "moduleDefinitions" [Definition] m,
      HasState "symTab" SymbolTable m
    ) ⇒
    m ()
defineMainPort = constantPort "main_port" 0
defineAuxiliary1 = constantPort "auxiliary1" 1
defineAuxiliary2 = constantPort "auxiliary2" 2
defineAuxiliary3 = constantPort "auxiliary3" 3
defineAuxiliary4 = constantPort "auxiliary4" 4

-- TODO :: abstract out hardcoded value!
constantPort ∷
  ( HasState "moduleDefinitions" [Definition] m,
    HasState "symTab" SymbolTable m
  ) ⇒
  Name →
  Integer →
  m ()
constantPort name v = do
  Block.addDefn
    $ GlobalDefinition
    $ Global.globalVariableDefaults
      { Global.name = name,
        Global.isConstant = True,
        Global.type' = Types.numPortsNameRef,
        Global.initializer = Just nodeValue
      }
  Block.assign
    (Block.nameToSymbol name)
    (ConstantOperand (C.GlobalReference (Types.pointerOf Types.numPortsNameRef) name))
  where
    nodeValue
      | Types.bitSizeEncodingPoint = C.Int Types.addressSpace v
      | otherwise =
        C.Struct
          { C.structName = Just Types.numPortsName,
            C.isPacked = True,
            C.memberValues =
              [ C.Int 1 0,
                C.Array
                  { C.memberType = Type.i16,
                    C.memberValues =
                      [ C.Int 16 v,
                        C.Int 16 0,
                        C.Int 16 0,
                        C.Int 16 0
                      ]
                  }
              ]
          }

mainPort,
  auxiliary1,
  auxiliary2,
  auxiliary3,
  auxiliary4 ∷
    Externf m ⇒ m Operand.Operand
mainPort = Block.externf "main_port"
auxiliary1 = Block.externf "auxiliary1"
auxiliary2 = Block.externf "auxiliary2"
auxiliary3 = Block.externf "auxiliary3"
auxiliary4 = Block.externf "auxiliary4"
