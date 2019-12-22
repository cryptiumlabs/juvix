-- | Operations necessary to update nodes
--
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
module Juvix.Backends.LLVM.Codegen.Graph where

import Juvix.Backends.LLVM.Codegen.Block as Block
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

defineLink ∷ Define m ⇒ m Operand.Operand
defineLink = Block.defineFunction Type.void "link" args $
  do
    setPort ("node_1", "port_1") ("node_2", "port_2")
    setPort ("node_2", "port_2") ("node_1", "port_1")
    retNull
  where
    args =
      [ (nodePointer, "node_1"),
        (numPortsPointer, "port_1"),
        (nodePointer, "node_2"),
        (numPortsPointer, "port_2")
      ]

-- perform offsets

defineIsBothPrimary ∷ Define m ⇒ m Operand.Operand
defineIsBothPrimary =
  Block.defineFunction (Types.pointerOf Types.bothPrimary) "is_both_primary" args $
    do
      -- TODO ∷ should this call be abstracted somewhere?!
      mainPort ← mainPort
      nodePtr ← Block.externf "node_ptr"
      portPtr ← findEdge [nodePtr, mainPort]
      otherNodePtr ← loadElementPtr $
        Types.Minimal
          { Types.type' = Types.nodePointer,
            Types.address' = portPtr,
            Types.indincies' = Block.constant32List [0, 0]
          }
      -- convert ptrs to ints
      nodeInt ← ptrToInt nodePtr pointerSize
      otherNodeInt ← ptrToInt otherNodePtr pointerSize
      -- compare the pointers to see if they are the same
      cmp ← icmp IntPred.EQ nodeInt otherNodeInt
      return' ← Block.alloca Types.bothPrimary
      tag ← getIsPrimaryEle return'
      nod ← getPrimaryNode Types.nodePointer return'
      store tag cmp
      store nod otherNodePtr
      ret return'
  where
    args = [(Types.nodePointer, "node_ptr")]

-- The logic assumes that the operation always succeeds
defineFindEdge ∷ Define m ⇒ m Operand.Operand
defineFindEdge =
  Block.defineFunction Types.portPointer "find_edge" args $
    do
      node ← Block.externf "node"
      pNum ← Block.externf "port"
      portPtr ← getPort node pNum
      otherPortPtr ← portPointsTo portPtr
      ret otherPortPtr
  where
    args = [(nodePointer, "node"), (numPortsPointer, "port")]

mallocNode ∷ Call m ⇒ Integer → m Operand.Operand
mallocNode size = Block.malloc size Types.nodePointer

-- H variants below mean that we are able to allocate from Haskell and
-- need not make a function

-- TODO ∷ could be storing data wrong... find out
mallocNodeH ∷
  ( RetInstruction m,
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
  let totalSize =
        Types.numPortsSize
          + (anyThere mPorts * Types.pointerSizeInt)
          + (anyThere mData * Types.pointerSizeInt)
          + extraData
  -- TODO ∷ see issue #262 on how to optimize out the loads and extra allocas
  nodePtr ← mallocNode (fromIntegral totalSize)
  portSizeP ← allocaNumPortNum (fromIntegral $ length mPorts)
  -- the bitCast is for turning the size of the array to 0
  -- for proper dynamically sized arrays
  ports ← mallocPortsH mPorts >>= flip bitCast Types.portData
  tagPtr ← getElementPtr $
    Types.Minimal
      { Types.type' = Types.numPortsPointer,
        Types.address' = nodePtr,
        Types.indincies' = Block.constant32List [0, 0]
      }
  portSize ← load numPortsNameRef portSizeP
  store tagPtr portSize
  portPtr ← getElementPtr $
    Types.Minimal
      { -- do I really want to say size 0?
        -- Yes, as we have to bitcast what get back to size 0!
        Types.type' = Types.pointerOf Types.portData,
        Types.address' = nodePtr,
        Types.indincies' = Block.constant32List [0, 1]
      }
  store portPtr ports
  -- let's not allocate data if we don't have to!
  case anyThere mData ∷ Integer of
    -- do this when we pass more information to deAllocateNode
    -- Until then it's not safe
    -- 0 → pure ()
    _ → do
      data' ← mallocDataH mData >>= flip bitCast Types.dataArray
      dataPtr ← getElementPtr $
        Types.Minimal
          { -- do I really want to say size 0?
            -- Yes, as we have to bitcast what get back to size 0!
            Types.type' = Types.pointerOf Types.dataArray,
            Types.address' = nodePtr,
            Types.indincies' = Block.constant32List [0, 2]
          }
      store dataPtr data'
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
            ptr ← getElementPtr $
              Types.Minimal
                { Types.type' = type',
                  Types.address' = ports,
                  Types.indincies' = Block.constant32List [0, i]
                }
            store ptr p
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
      oldPointsTo ← findEdge [nOld, pOld]
      let intoGen typ num = getElementPtr $
            Types.Minimal
              { Types.type' = typ,
                Types.address' = oldPointsTo,
                Types.indincies' = Block.constant32List [0, num]
              }
      numPointsTo ← intoGen numPortsPointer 1
      nodePointsToPtr ← intoGen (Types.pointerOf nodePointer) 0 >>= load nodePointer
      -- End Abstracting out bits -------------------------------------------------
      _ ← link [nNew, pNew, nodePointsToPtr, numPointsTo]
      retNull
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
      let intoGen typ num = getElementPtr $
            Types.Minimal
              { Types.type' = typ,
                Types.address' = oldPointsTo,
                Types.indincies' = Block.constant32List [0, num]
              }
      numPointsTo ← intoGen numPortsPointer 1
      nodePointsToPtr ← intoGen (Types.pointerOf nodePointer) 0 >>= load nodePointer
      -- End Abstracting out bits -------------------------------------------------
      _ ← linkConnectedPort [n2, p2, nodePointsToPtr, numPointsTo]
      retNull
  where
    args =
      [ (nodePointer, "node_one"),
        (numPortsPointer, "port_one"),
        (nodePointer, "node_two"),
        (numPortsPointer, "port_two")
      ]

deAllocateNode ∷ Define m ⇒ Operand.Operand → m ()
deAllocateNode nodePtr = do
  portPtr ← loadElementPtr $
    Types.Minimal
      { Types.type' = Types.portData,
        Types.address' = nodePtr,
        Types.indincies' = Block.constant32List [0, 1]
      }
  dataPtr ← loadElementPtr $
    Types.Minimal
      { Types.type' = Types.dataArray,
        Types.address' = nodePtr,
        Types.indincies' = Block.constant32List [0, 2]
      }
  _ ← free portPtr
  _ ← free dataPtr
  free nodePtr

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- Logic for setPort expanded

-- 1. getElementPointer the tag
-- Int ⇒
-- \ 1. times offset by this number (* Don't really have to do it! *)
-- \ 2. grab the node from here
-- \ 3. do operation
-- Int* ⇒
-- \ 1. deref the Int
-- \ 2. times the offset by this deref (* Don't really have to do it! *)
-- \ 3. grab node from  here
-- \ 4. do operation

setPort ∷
  ( Call m,
    HasState "blockCount" Int m,
    HasState "names" Names m
  ) ⇒
  (Name.Name, Name.Name) →
  (Name.Name, Name.Name) →
  m ()
setPort (n1, p1) (n2, p2) = do
  (no1, po1) ← (,) <$> Block.externf n1 <*> Block.externf p1
  (no2, po2) ← (,) <$> Block.externf n2 <*> Block.externf p2
  -- Grab the port pointer of no1 at port p1
  portLocation ← getPort no1 po1
  -- set the port to be no1 and po1
  setPortType portLocation no2 po2

setPortType ∷
  Call m ⇒ Operand.Operand → Operand.Operand → Operand.Operand → m ()
setPortType portPtr node givenOffsetPtr = do
  nodePtr ← getElementPtr $
    Types.Minimal
      { Types.type' = Types.pointerOf nodePointer,
        Types.address' = portPtr,
        Types.indincies' = Block.constant32List [0, 0]
      }
  offsetPtr ← getElementPtr $
    Types.Minimal
      { Types.type' = numPortsPointer,
        Types.address' = portPtr,
        Types.indincies' = Block.constant32List [0, 1]
      }
  -- now store the pointer to the Newport
  store nodePtr node
  -- store the offset
  offset ← load Types.numPortsNameRef givenOffsetPtr
  store offsetPtr offset
  pure ()

-- | 'getPort' takes a node* and a numPort* for operands and gives back a port*
getPort ∷
  ( RetInstruction m,
    HasState "blockCount" Int m,
    HasState "names" Names m
  ) ⇒
  Operand.Operand →
  Operand.Operand →
  m Operand.Operand
getPort node port = do
  portsPtrPtr ← getElementPtr $
    Types.Minimal
      { Types.type' = Types.pointerOf portData,
        Types.address' = node,
        Types.indincies' = Block.constant32List [0, 1]
      }
  portsPtr ← load portData portsPtrPtr
  intOfNumPorts portPointer port $ \value → do
    getElementPtr $
      Types.Minimal
        { Types.type' = portPointer,
          Types.address' = portsPtr,
          Types.indincies' =
            [ Operand.ConstantOperand (C.Int 32 0),
              value
            ]
        }

-- | 'intOfNumPorts' generates an int of two different sizes to be used in cont logic
-- the type referees to the final type in the cont logic
intOfNumPorts ∷
  ( RetInstruction m,
    HasState "blockCount" Int m,
    HasState "names" Names m
  ) ⇒
  Type.Type →
  Operand.Operand →
  (Operand.Operand → m Operand.Operand) →
  m Operand.Operand
intOfNumPorts typ numPort cont = do
  -- grab the tag from the numPort
  tag ← Block.loadElementPtr $
    Types.Minimal
      { Types.type' = Type.i1,
        Types.address' = numPort,
        Types.indincies' = Block.constant32List [0, 0]
      }
  generateIf typ tag largeBranch smallBranch
  where
    smallBranch = branchGen numPortsSmall numPortsSmallValue return

    largeBranch = branchGen numPortsLarge numPortsLargeValuePtr $
      \vPtr →
        Block.loadElementPtr $
          Types.Minimal
            { Types.type' = numPortsLargeValue,
              Types.address' = vPtr,
              Types.indincies' = Block.constant32List [0]
            }

    -- Generic logic
    branchGen variant variantType extraDeref = do
      casted ← bitCast numPort (Types.pointerOf (varientToType variant))
      value ← Block.loadElementPtr $
        Types.Minimal
          { Types.type' = variantType,
            Types.address' = casted,
            Types.indincies' = Block.constant32List [0, 1]
          }
      -- Does nothing for the small case
      value ← extraDeref value
      cont value

-- | 'portPointsTo' is a function which grabs the portType of the node a portType points to
portPointsTo ∷
  ( RetInstruction m,
    HasState "blockCount" Int m,
    HasState "names" Names m
  ) ⇒
  Operand.Operand →
  m Operand.Operand
portPointsTo portType = do
  -- TODO ∷ see if there is any special logic for packed Types
  -- Do I have to index by size?
  nodePtr ← Block.loadElementPtr $
    Types.Minimal
      { Types.type' = nodePointer,
        Types.address' = portType,
        Types.indincies' = Block.constant32List [0, 0]
      }
  -- Get the numPort
  numPort ← Block.getElementPtr $
    Types.Minimal
      { Types.type' = numPortsPointer,
        Types.address' = portType,
        -- Index may change due to being packed
        Types.indincies' = Block.constant32List [0, 1]
      }
  getPort nodePtr numPort

-- | Allocates a 'numPorts'
createNumPortsStaticGen ∷
  ( RetInstruction m,
    HasState "typTab" TypeTable m,
    HasState "varTab" VariantToType m
  ) ⇒
  Bool →
  Operand.Operand →
  (Symbol → [Operand.Operand] → Integer → m Operand.Operand) →
  (Type.Type → Integer → m Operand.Operand) →
  m Operand.Operand
createNumPortsStaticGen isLarge value allocVar alloc =
  -- Call Block.createVariant
  -- Issue is that I need to register this sum type in the map
  -- else it is an error.
  -- see if this is okay, if not make custom logic just for the
  -- sums to create the language
  let castIt x = Block.bitCast x (Types.pointerOf Types.numPortsNameRef)
   in case isLarge of
        False → do
          small ← allocVar "numPorts_small" [value] Types.numPortsSize
          castIt small
        True → do
          -- Allocate a pointer to the value
          ptr ← alloc nodePointer Types.nodePointerSize
          store ptr value
          large ← allocVar "numPorts_large" [ptr] Types.numPortsSize
          castIt large

-- | Allocates 'numPorts' via allcoca
allocaNumPortsStatic ∷
  ( RetInstruction m,
    HasState "typTab" TypeTable m,
    HasState "varTab" VariantToType m
  ) ⇒
  Bool →
  Operand.Operand →
  m Operand.Operand
allocaNumPortsStatic isLarge value =
  createNumPortsStaticGen
    isLarge
    value
    (\s xs _ → Block.allocaVariant s xs)
    (const . alloca)

-- | Allocates 'numPorts' via allcoca
mallocNumPortsStatic ∷
  Types.MallocNode m ⇒ Bool → Operand.Operand → m Operand.Operand
mallocNumPortsStatic isLarge value =
  -- we do . pointerOf here, as the malloc version sets the type to a * to it
  -- unlike the alloca which we just need to say the type itself
  createNumPortsStaticGen
    isLarge
    value
    Block.mallocVariant
    (flip Block.malloc . pointerOf)

createNumPortNumGen ∷ Integer → (Bool → Operand.Operand → p) → p
createNumPortNumGen n alloc
  | n <= 2 ^ (Types.numPortsSize - 1 ∷ Integer) =
    alloc False (Operand.ConstantOperand (C.Int Types.pointerSizeInt n))
  | otherwise =
    alloc True (Operand.ConstantOperand (C.Int Types.numPortsLargeValueInt n))

-- | like 'allocaNumPortStatic', except it takes a number and allocates the correct operand
allocaNumPortNum ∷
  Types.AllocaNode m ⇒ Integer → m Operand.Operand
allocaNumPortNum n = createNumPortNumGen n allocaNumPortsStatic

-- | like 'mallocNumPortStatic', except it takes a number and allocates the correct operand
mallocNumPortNum ∷
  Types.MallocNode m ⇒ Integer → m Operand.Operand
mallocNumPortNum n = createNumPortNumGen n mallocNumPortsStatic

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
  addDefn
    $ GlobalDefinition
    $ Global.globalVariableDefaults
      { Global.name = name,
        Global.isConstant = True,
        Global.type' = Types.numPortsNameRef,
        Global.initializer = Just C.Struct
          { C.structName = Just Types.numPortsName,
            C.isPacked = True,
            C.memberValues =
              [ C.Int 1 0,
                C.Array
                  { C.memberType = Type.i16,
                    C.memberValues =
                      [ C.Int 16 0,
                        C.Int 16 0,
                        C.Int 16 0,
                        C.Int 16 v
                      ]
                  }
              ]
          }
      }
  assign
    (Block.nameToSymbol name)
    (ConstantOperand (C.GlobalReference (Types.pointerOf Types.numPortsNameRef) name))

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

--------------------------------------------------------------------------------
-- Accessor aliases
--------------------------------------------------------------------------------

getIsPrimaryEle ∷ RetInstruction m ⇒ Operand.Operand → m Operand.Operand
getIsPrimaryEle bothPrimary =
  getElementPtr $
    Types.Minimal
      { Types.type' = Types.pointerOf Type.i1,
        Types.address' = bothPrimary,
        Types.indincies' = Block.constant32List [0, 0]
      }

loadIsPrimaryEle ∷ RetInstruction m ⇒ Operand.Operand → m Operand.Operand
loadIsPrimaryEle e = getIsPrimaryEle e >>= load Type.i1

getPrimaryNode ∷ RetInstruction m ⇒ Type.Type → Operand.Operand → m Operand.Operand
getPrimaryNode nodePtrType bothPrimary =
  getElementPtr $
    Types.Minimal
      { Types.type' = Types.pointerOf nodePtrType,
        Types.address' = bothPrimary,
        Types.indincies' = Block.constant32List [0, 1]
      }

loadPrimaryNode ∷ RetInstruction m ⇒ Type.Type → Operand.Operand → m Operand.Operand
loadPrimaryNode nodePtrType e = getPrimaryNode nodePtrType e >>= load nodePtrType
