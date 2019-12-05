-- | Operations necessary to update nodes
module Juvix.Backends.LLVM.Codegen.Graph where

import Juvix.Backends.LLVM.Codegen.Block as Block
import Juvix.Backends.LLVM.Codegen.Types as Types
import Juvix.Library hiding (Type, local)
import qualified Juvix.Library.HashMap as Map
import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.IntegerPredicate as IntPred
import qualified LLVM.AST.Name as Name
import qualified LLVM.AST.Operand as Operand
import qualified LLVM.AST.Type as Type

--------------------------------------------------------------------------------
-- Call Alias for Main Functions
--------------------------------------------------------------------------------

callGen ∷
  ( HasThrow "err" Errors m,
    HasState "blocks" (Map.HashMap Name.Name BlockState) m,
    HasState "count" Word m,
    HasState "currentBlock" Name.Name m,
    HasState "symtab" SymbolTable m
  ) ⇒
  Type.Type →
  [Operand.Operand] →
  Name.Name →
  m Operand.Operand
callGen typ' args fn = do
  f ← Block.externf fn
  Block.call typ' f (Block.emptyArgs args)

link,
  linkConnectedPort,
  rewire ∷
    ( HasThrow "err" Errors m,
      HasState "blocks" (Map.HashMap Name.Name BlockState) m,
      HasState "count" Word m,
      HasState "currentBlock" Name.Name m,
      HasState "symtab" SymbolTable m
    ) ⇒
    [Operand.Operand] →
    m ()
link args = callGen Type.void args "link" >> pure ()
rewire args = callGen Type.void args "rewire" >> pure ()
linkConnectedPort args = callGen Type.void args "link_connected_port" >> pure ()

isBothPrimary,
  findEdge ∷
    ( HasThrow "err" Errors m,
      HasState "blocks" (Map.HashMap Name.Name BlockState) m,
      HasState "count" Word m,
      HasState "currentBlock" Name.Name m,
      HasState "symtab" SymbolTable m
    ) ⇒
    Type.Type →
    [Operand.Operand] →
    m Operand.Operand
isBothPrimary typ args = callGen (Types.bothPrimary typ) args "is_both_primary"
findEdge typ args = callGen typ args "find_edge"

--------------------------------------------------------------------------------
-- Main Functions
--------------------------------------------------------------------------------

-- TODO ∷ delNodes, deleteRewire, deleteEdge, allocaNode in full

-- TODO ∷ abstract over the define pattern seen below?

link' ∷
  ( HasThrow "err" Errors m,
    HasState "blockCount" Int m,
    HasState "blocks" (Map.HashMap Name.Name BlockState) m,
    HasState "count" Word m,
    HasState "currentBlock" Name.Name m,
    HasState "moduleDefinitions" [AST.Definition] m,
    HasState "names" Names m,
    HasState "symtab" (Map.HashMap Symbol Operand.Operand) m
  ) ⇒
  Type.Type →
  m Operand.Operand
link' nodePtrType = Block.defineFunction Type.void "link" args $
  do
    setPort ("node_1", "port_1") ("node_2", "port_2") nodePtrType
    setPort ("node_2", "port_2") ("node_1", "port_1") nodePtrType
    retNull
  where
    args =
      [ (nodeType nodePtrType, "node_1"),
        (numPorts, "port_1"),
        (nodeType nodePtrType, "node_2"),
        (numPorts, "port_2")
      ]

-- perform offsets

isBothPrimary' ∷
  ( HasThrow "err" Errors m,
    HasState "blockCount" Int m,
    HasState "blocks" (Map.HashMap Name.Name BlockState) m,
    HasState "count" Word m,
    HasState "currentBlock" Name.Name m,
    HasState "moduleDefinitions" [AST.Definition] m,
    HasState "names" Names m,
    HasState "symtab" (Map.HashMap Symbol Operand.Operand) m,
    HasState "typTab" TypeTable m,
    HasState "varTab" VariantToType m
  ) ⇒
  Type.Type →
  m Operand.Operand
isBothPrimary' nodePtrTyp =
  Block.defineFunction (Types.bothPrimary nodePtrTyp) "is_both_primary" args $
    do
      -- TODO ∷ should this call be abstracted somewhere?!
      -- Why should Ι allocate for every port?!
      mainPort ← mainPort nodePtrTyp
      -- TODO ∷ Make sure findEdge is in the environment
      edge ← Block.externf "find_edge"
      nodePtr ← Block.externf "node_ptr"
      node ← load (nodeType nodePtrTyp) nodePtr
      port ← call (portType nodePtrTyp) edge (Block.emptyArgs [node, mainPort])
      otherNodePtr ← loadElementPtr $
        Types.Minimal
          { Types.type' = nodePointer nodePtrTyp,
            Types.address' = port,
            Types.indincies' = Block.constant32List [0, 0]
          }
      -- convert ptrs to ints
      nodeInt ← ptrToInt nodePtr pointerSize
      otherNodeInt ← ptrToInt otherNodePtr pointerSize
      -- compare the pointers to see if they are the same
      cmp ← icmp IntPred.EQ nodeInt otherNodeInt
      return' ← Block.alloca (Types.bothPrimary nodePtrTyp)
      tag ← getIsPrimaryEle return'
      nod ← getPrimaryNode nodePtrTyp return'
      store tag cmp
      store nod otherNodePtr
      ret return'
  where
    args = [(nodePtrTyp, "node_ptr")]

-- The logic assumes that the operation always succeeds
findEdge' ∷
  ( HasThrow "err" Errors m,
    HasState "blockCount" Int m,
    HasState "blocks" (Map.HashMap Name.Name BlockState) m,
    HasState "count" Word m,
    HasState "currentBlock" Name.Name m,
    HasState "moduleDefinitions" [AST.Definition] m,
    HasState "names" Names m,
    HasState "symtab" (Map.HashMap Symbol Operand.Operand) m
  ) ⇒
  Type.Type →
  m Operand.Operand
findEdge' nodePtrType = Block.defineFunction nodePtrType "find_edge" args $
  do
    node ← Block.externf "node"
    pNum ← Block.externf "port"
    portPtr ← getPort node pNum nodePtrType
    port ← load (portType nodePtrType) portPtr
    otherPtr ← portPointsTo port nodePtrType
    other ← load (portType nodePtrType) otherPtr
    ret other
  where
    args = [(nodeType nodePtrType, "node"), (numPorts, "port")]

-- TODO ∷ allocaPorts, allocaPortType, allocaData, allocaNodeType
-- Name           | Arguments
-- allocaNodeType : two array args
-- allocaPortType : nodePointer ∧ numPorts
-- allocaPorts    : variable args of ports
-- allocaData     : variable args of dataType

mallocNode ∷
  ( HasThrow "err" Errors m,
    HasState "blocks" (Map.HashMap Name.Name BlockState) m,
    HasState "count" Word m,
    HasState "currentBlock" Name.Name m,
    HasState "symtab" SymbolTable m
  ) ⇒
  Type.Type →
  Integer →
  m Operand.Operand
mallocNode t size = Block.mallocType size (nodeType t)

-- H variants below mean that we are able to allocate from Haskell and
-- need not make a function

-- TODO ∷ could be storing data wrong... find out
mallocNodeH ∷
  ( HasThrow "err" Errors m,
    HasState "blocks" (Map.HashMap Name.Name BlockState) m,
    HasState "count" Word m,
    HasState "currentBlock" Name.Name m,
    HasState "typTab" TypeTable m,
    HasState "varTab" VariantToType m,
    HasState "symtab" SymbolTable m
  ) ⇒
  [Maybe Operand.Operand] →
  [Maybe Operand.Operand] →
  Type.Type →
  Integer →
  m Operand.Operand
mallocNodeH mPorts mData nodePtrType nodeSize = do
  node ← mallocNode nodePtrType nodeSize
  portSize ← mallocNumPortNum (fromIntegral $ length mPorts) nodePtrType
  ports ← mallocPortsH mPorts nodePtrType
  data' ← mallocDataH mData
  tagPtr ← getElementPtr $
    Types.Minimal
      { Types.type' = Types.numPorts,
        Types.address' = node,
        Types.indincies' = Block.constant32List [0, 0]
      }
  store tagPtr portSize
  portPtr ← getElementPtr $
    Types.Minimal
      { Types.type' = Types.portData nodePtrType, -- do I really want to say size 0?
        Types.address' = node,
        Types.indincies' = Block.constant32List [0, 1]
      }
  store portPtr ports
  dataPtr ← getElementPtr $
    Types.Minimal
      { Types.type' = Type.ArrayType 0 Types.dataType, -- do I really want to say size 0?
        Types.address' = node,
        Types.indincies' = Block.constant32List [0, 2]
      }
  store dataPtr data'
  pure node

-- | used to create the malloc and alloca functions for ports and data
createGenH ∷
  ( HasThrow "err" Errors m,
    HasState "blocks" (Map.HashMap Name.Name BlockState) m,
    HasState "count" Word m,
    HasState "currentBlock" Name.Name m
  ) ⇒
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
  ( HasThrow "err" Errors m,
    HasState "blocks" (Map.HashMap Name.Name BlockState) m,
    HasState "count" Word m,
    HasState "currentBlock" Name.Name m,
    HasState "symtab" SymbolTable m
  ) ⇒
  [Maybe Operand.Operand] →
  Type.Type →
  Integer →
  m Operand.Operand
mallocGenH mPortData type' dataSize =
  createGenH mPortData type' (\t len → Block.mallocType (len * dataSize) t)

allocaGenH ∷
  ( HasThrow "err" Errors m,
    HasState "blocks" (Map.HashMap Name.Name BlockState) m,
    HasState "count" Word m,
    HasState "currentBlock" Name.Name m
  ) ⇒
  [Maybe Operand.Operand] →
  Type.Type →
  m Operand.Operand
allocaGenH mPortData type' = createGenH mPortData type' (const . Block.alloca)

mallocPortsH ∷
  ( HasThrow "err" Errors m,
    HasState "blocks" (Map.HashMap Name.Name BlockState) m,
    HasState "count" Word m,
    HasState "currentBlock" Name.Name m,
    HasState "symtab" SymbolTable m
  ) ⇒
  [Maybe Operand.Operand] →
  Type.Type →
  m Operand.Operand
mallocPortsH mPorts typ = mallocGenH mPorts (Types.portType typ) Types.portTypeSize

allocaPortsH ∷
  ( HasThrow "err" Errors m,
    HasState "blocks" (Map.HashMap Name.Name BlockState) m,
    HasState "count" Word m,
    HasState "currentBlock" Name.Name m
  ) ⇒
  [Maybe Operand.Operand] →
  Type.Type →
  m Operand.Operand
allocaPortsH mPorts = allocaGenH mPorts . Types.portType

allocaDataH ∷
  ( HasThrow "err" Errors m,
    HasState "blocks" (Map.HashMap Name.Name BlockState) m,
    HasState "count" Word m,
    HasState "currentBlock" Name.Name m
  ) ⇒
  [Maybe Operand.Operand] →
  m Operand.Operand
allocaDataH mPorts = allocaGenH mPorts Types.dataType

mallocDataH ∷
  ( HasThrow "err" Errors m,
    HasState "blocks" (Map.HashMap Name.Name BlockState) m,
    HasState "count" Word m,
    HasState "currentBlock" Name.Name m,
    HasState "symtab" SymbolTable m
  ) ⇒
  [Maybe Operand.Operand] →
  m Operand.Operand
mallocDataH mPorts = mallocGenH mPorts Types.dataType Types.dataTypeSize

-- derived from the core functions

linkConnectedPort' ∷
  ( HasThrow "err" Errors m,
    HasState "blockCount" Int m,
    HasState "blocks" (Map.HashMap Name.Name BlockState) m,
    HasState "count" Word m,
    HasState "currentBlock" Name.Name m,
    HasState "moduleDefinitions" [AST.Definition] m,
    HasState "names" Names m,
    HasState "symtab" SymbolTable m
  ) ⇒
  Type.Type →
  m Operand.Operand
linkConnectedPort' nodePtrType =
  Block.defineFunction Type.void "link_connected_port" args $
    do
      edge ← Block.externf "find_edge"
      link ← Block.externf "link"
      (nOld, pOld) ← (,) <$> Block.externf "node_old" <*> Block.externf "port_old"
      (nNew, pNew) ← (,) <$> Block.externf "node_new" <*> Block.externf "port_new"
      oldPointsTo ← call (Types.portType nodePtrType) edge (Block.emptyArgs [nOld, pOld])
      -- TODO ∷ Abstract out this bit ---------------------------------------------
      let intoGen typ num = loadElementPtr $
            Types.Minimal
              { Types.type' = typ,
                Types.address' = oldPointsTo,
                Types.indincies' = Block.constant32List [0, num]
              }
      numPointsTo ← intoGen numPorts 1
      nodePointsToPtr ← intoGen (nodePointer nodePtrType) 0
      nodePointsTo ← load (nodeType nodePtrType) nodePointsToPtr
      -- End Abstracting out bits -------------------------------------------------
      _ ← call Type.void link (Block.emptyArgs [nNew, pNew, numPointsTo, nodePointsTo])
      retNull
  where
    args =
      [ (nodeType nodePtrType, "node_old"),
        (numPorts, "port_old"),
        (nodeType nodePtrType, "node_new"),
        (numPorts, "port_new")
      ]

rewire' ∷
  ( HasThrow "err" Errors m,
    HasState "blockCount" Int m,
    HasState "blocks" (Map.HashMap Name.Name BlockState) m,
    HasState "count" Word m,
    HasState "currentBlock" Name.Name m,
    HasState "moduleDefinitions" [AST.Definition] m,
    HasState "names" Names m,
    HasState "symtab" SymbolTable m
  ) ⇒
  Type.Type →
  m Operand.Operand
rewire' nodePtrType = Block.defineFunction Type.void "rewire" args $
  do
    edge ← Block.externf "find_edge"
    relink ← Block.externf "link_connected_port"
    -- TODO ∷ Abstract out this bit ---------------------------------------------
    (n1, p1) ← (,) <$> Block.externf "node_one" <*> Block.externf "port_one"
    (n2, p2) ← (,) <$> Block.externf "node_two" <*> Block.externf "port_two"
    oldPointsTo ← call (Types.portType nodePtrType) edge (Block.emptyArgs [n1, p1])
    let intoGen typ num = loadElementPtr $
          Types.Minimal
            { Types.type' = typ,
              Types.address' = oldPointsTo,
              Types.indincies' = Block.constant32List [0, num]
            }
    numPointsTo ← intoGen numPorts 1
    nodePointsToPtr ← intoGen (nodePointer nodePtrType) 0
    nodePointsTo ← load (nodeType nodePtrType) nodePointsToPtr
    -- End Abstracting out bits -------------------------------------------------
    _ ← call Type.void relink (Block.emptyArgs [n2, p2, numPointsTo, nodePointsTo])
    retNull
  where
    args =
      [ (nodeType nodePtrType, "node_one"),
        (numPorts, "port_one"),
        (nodeType nodePtrType, "node_two"),
        (numPorts, "port_two")
      ]

delNode = undefined

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
  ( HasThrow "err" Errors m,
    HasState "blockCount" Int m,
    HasState "blocks" (Map.HashMap Name.Name BlockState) m,
    HasState "count" Word m,
    HasState "currentBlock" Name.Name m,
    HasState "names" Names m,
    HasState "symtab" SymbolTable m
  ) ⇒
  (Name.Name, Name.Name) →
  (Name.Name, Name.Name) →
  Type.Type →
  m ()
setPort (n1, p1) (n2, p2) nodePtrType = do
  (no1, po1) ← (,) <$> Block.externf n1 <*> Block.externf p1
  (no2, po2) ← (,) <$> Block.externf n2 <*> Block.externf p2
  -- Grab the int value of port 1
  portLocation ← getPort no1 po1 nodePtrType
  p2Ptr ← newPortType no2 po2 nodePtrType
  store portLocation p2Ptr
  pure ()

-- TODO ∷ should be malloc
newPortType ∷
  ( HasThrow "err" Errors m,
    HasState "blocks" (Map.HashMap Name.Name BlockState) m,
    HasState "count" Word m,
    HasState "currentBlock" Name.Name m,
    HasState "symtab" SymbolTable m
  ) ⇒
  Operand.Operand →
  Operand.Operand →
  Type.Type →
  m Operand.Operand
newPortType node offset nodePtrType = do
  newPort ← Block.mallocType Types.portTypeSize (Types.portType nodePtrType)
  -- This is a ptr to a ptr
  nodePtr ← getElementPtr $
    Types.Minimal
      { Types.type' = nodePointer nodePtrType,
        Types.address' = newPort,
        Types.indincies' = Block.constant32List [0, 0]
      }
  offsetPtr ← getElementPtr $
    Types.Minimal
      { Types.type' = numPorts,
        Types.address' = newPort,
        Types.indincies' = Block.constant32List [0, 1]
      }
  -- allocate pointer to the node
  givenNodePtr ← Block.mallocType Types.nodePointerSize (Types.nodePointer nodePtrType)
  placeToStoreNode ← getElementPtr $
    Types.Minimal
      { Types.type' = nodeType nodePtrType,
        Types.address' = givenNodePtr,
        Types.indincies' = Block.constant32List [0, 0]
      }
  -- Store the node to it
  store placeToStoreNode node
  -- now store the pointer to the newPort
  store nodePtr givenNodePtr
  -- store the offset
  store offsetPtr offset
  pure newPort

getPort ∷
  ( HasThrow "err" Errors m,
    HasState "blockCount" Int m,
    HasState "blocks" (Map.HashMap Name.Name BlockState) m,
    HasState "count" Word m,
    HasState "currentBlock" Name.Name m,
    HasState "names" Names m
  ) ⇒
  Operand.Operand →
  Operand.Operand →
  Type.Type →
  m Operand.Operand
getPort node port nodePtrType = do
  intOfNumPorts (portPointer nodePtrType) port $ \value → do
    ports ← loadElementPtr $
      Types.Minimal
        { Types.type' = portData nodePtrType,
          Types.address' = node,
          Types.indincies' = Block.constant32List [0, 1]
        }
    -- allocate the new pointer
    getElementPtr $
      Types.Minimal
        { Types.type' = portType nodePtrType,
          Types.address' = ports,
          -- TODO ∷ Ι may have to count size here, I don't think so?
          Types.indincies' =
            [ Operand.ConstantOperand (C.Int 32 0),
              value
            ]
        }

-- | 'intOfNumPorts' generates an int of two different sizes to be used in cont logic
-- the type referees to the final type in the cont logic
intOfNumPorts ∷
  ( HasThrow "err" Errors m,
    HasState "blockCount" Int m,
    HasState "blocks" (Map.HashMap Name.Name BlockState) m,
    HasState "count" Word m,
    HasState "currentBlock" Name.Name m,
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
  generateIf typ tag smallBranch largeBranch
  where
    smallBranch = branchGen numPortsSmall numPortsSmallValue return

    largeBranch = branchGen numPortsLarge numPortsLargeValuePtr $
      \vPtr →
        Block.loadElementPtr $
          Types.Minimal
            { Types.type' = numPortsLargeValue,
              Types.address' = vPtr,
              Types.indincies' = Block.constant32List [0, 0]
            }

    -- Generic logic
    branchGen variant variantType extraDeref = do
      casted ← bitCast numPort (varientToType variant)
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
  ( HasThrow "err" Errors m,
    HasState "blockCount" Int m,
    HasState "blocks" (Map.HashMap Name.Name BlockState) m,
    HasState "count" Word m,
    HasState "currentBlock" Name.Name m,
    HasState "names" Names m
  ) ⇒
  Operand.Operand →
  Type.Type →
  m Operand.Operand
portPointsTo portType nodePtrType = do
  -- TODO ∷ see if there is any special logic for packed Types
  -- Do I have to index by size?
  nodePtr ← Block.loadElementPtr $
    Types.Minimal
      { Types.type' = nodePointer nodePtrType,
        Types.address' = portType,
        Types.indincies' = Block.constant32List [0, 0]
      }
  -- get the node which it points to
  nodeType ← loadElementPtr $
    Types.Minimal
      { Types.type' = nodeType nodePtrType,
        Types.address' = nodePtr,
        Types.indincies' = Block.constant32List [0, 0]
      }
  -- Get the numPort
  numPort ← Block.loadElementPtr $
    Types.Minimal
      { Types.type' = numPorts,
        Types.address' = portType,
        -- Index may change due to being packed
        Types.indincies' = Block.constant32List [0, 1]
      }
  getPort nodeType numPort nodePtrType

-- | Allocates a 'numPorts'
createNumPortsStaticGen ∷
  ( HasThrow "err" Errors m,
    HasState "blocks" (Map.HashMap Name.Name BlockState) m,
    HasState "count" Word m,
    HasState "currentBlock" Name.Name m,
    HasState "typTab" TypeTable m,
    HasState "varTab" VariantToType m
  ) ⇒
  Bool →
  Operand.Operand →
  Type.Type →
  (Symbol → [Operand.Operand] → Integer → m Operand.Operand) →
  (Type.Type → Integer → m Operand.Operand) →
  m Operand.Operand
createNumPortsStaticGen isLarge value nodePtrType allocVar alloc =
  -- Call Block.createVariant
  -- Issue is that I need to register this sum type in the map
  -- else it is an error.
  -- see if this is okay, if not make custom logic just for the
  -- sums to create the language
  case isLarge of
    False →
      -- TODO ∷ register this variant
      allocVar "numPorts_small" [value] Types.numPortsSize
    True → do
      -- Allocate a pointer to the value
      ptr ← alloc (nodePointer nodePtrType) Types.nodePointerSize
      store ptr value
      -- TODO ∷ register this variant
      allocVar "numPorts_large" [ptr] Types.numPortsSize

-- | Allocates 'numPorts' via allcoca
allocaNumPortsStatic ∷
  ( HasThrow "err" Errors m,
    HasState "blocks" (Map.HashMap Name.Name BlockState) m,
    HasState "count" Word m,
    HasState "currentBlock" Name.Name m,
    HasState "typTab" TypeTable m,
    HasState "varTab" VariantToType m
  ) ⇒
  Bool →
  Operand.Operand →
  Type.Type →
  m Operand.Operand
allocaNumPortsStatic isLarge value nodePtrType =
  createNumPortsStaticGen
    isLarge
    value
    nodePtrType
    (\s xs _ → Block.allocaVariant s xs)
    (const . alloca)

-- | Allocates 'numPorts' via allcoca
mallocNumPortsStatic ∷
  ( HasThrow "err" Errors m,
    HasState "blocks" (Map.HashMap Name.Name BlockState) m,
    HasState "count" Word m,
    HasState "currentBlock" Name.Name m,
    HasState "typTab" TypeTable m,
    HasState "varTab" VariantToType m,
    HasState "symtab" SymbolTable m
  ) ⇒
  Bool →
  Operand.Operand →
  Type.Type →
  m Operand.Operand
mallocNumPortsStatic isLarge value nodePtrType =
  createNumPortsStaticGen isLarge value nodePtrType Block.mallocVariant (flip mallocType)

createNumPortNumGen ∷ Integer → t → (Bool → Operand.Operand → t → p) → p
createNumPortNumGen n nodePtrType alloc
  | n <= 2 ^ (Types.numPortsSize - 1 ∷ Integer) =
    alloc False (Operand.ConstantOperand (C.Int 16 n)) nodePtrType
  | otherwise =
    alloc True (Operand.ConstantOperand (C.Int 64 n)) nodePtrType

-- | like 'allocaNumPortStatic', except it takes a number and allocates the correct operand
allocaNumPortNum ∷
  ( HasThrow "err" Errors m,
    HasState "blocks" (Map.HashMap Name.Name BlockState) m,
    HasState "count" Word m,
    HasState "currentBlock" Name.Name m,
    HasState "typTab" TypeTable m,
    HasState "varTab" VariantToType m
  ) ⇒
  Integer →
  Type.Type →
  m Operand.Operand
allocaNumPortNum n nodePtrType = createNumPortNumGen n nodePtrType allocaNumPortsStatic

-- | like 'mallocNumPortStatic', except it takes a number and allocates the correct operand
mallocNumPortNum ∷
  ( HasThrow "err" Errors m,
    HasState "blocks" (Map.HashMap Name.Name BlockState) m,
    HasState "count" Word m,
    HasState "currentBlock" Name.Name m,
    HasState "symtab" SymbolTable m,
    HasState "typTab" TypeTable m,
    HasState "varTab" VariantToType m
  ) ⇒
  Integer →
  Type.Type →
  m Operand.Operand
mallocNumPortNum n nodePtrType = createNumPortNumGen n nodePtrType mallocNumPortsStatic

--------------------------------------------------------------------------------
-- Port Aliases
--------------------------------------------------------------------------------

-- TODO ∷ put these in the env, and allocate them once at the top level

mainPort,
  auxiliary1,
  auxiliary2,
  auxiliary3,
  auxiliary4 ∷
    ( HasThrow "err" Errors m,
      HasState "blocks" (Map.HashMap Name.Name BlockState) m,
      HasState "count" Word m,
      HasState "currentBlock" Name.Name m,
      HasState "typTab" TypeTable m,
      HasState "varTab" VariantToType m
    ) ⇒
    Type.Type →
    m Operand.Operand
mainPort = allocaNumPortsStatic False (Operand.ConstantOperand (C.Int 32 0))
auxiliary1 = allocaNumPortsStatic False (Operand.ConstantOperand (C.Int 32 1))
auxiliary2 = allocaNumPortsStatic False (Operand.ConstantOperand (C.Int 32 2))
auxiliary3 = allocaNumPortsStatic False (Operand.ConstantOperand (C.Int 32 3))
auxiliary4 = allocaNumPortsStatic False (Operand.ConstantOperand (C.Int 32 4))

--------------------------------------------------------------------------------
-- Accessor aliases
--------------------------------------------------------------------------------

getIsPrimaryEle ∷
  ( HasThrow "err" Errors m,
    HasState "blocks" (Map.HashMap Name.Name BlockState) m,
    HasState "count" Word m,
    HasState "currentBlock" Name.Name m
  ) ⇒
  Operand.Operand →
  m Operand.Operand
getIsPrimaryEle bothPrimary =
  getElementPtr $
    Types.Minimal
      { Types.type' = Type.i1,
        Types.address' = bothPrimary,
        Types.indincies' = Block.constant32List [0, 0]
      }

loadIsPrimaryEle ∷
  ( HasThrow "err" Errors m,
    HasState "blocks" (Map.HashMap Name.Name BlockState) m,
    HasState "count" Word m,
    HasState "currentBlock" Name.Name m
  ) ⇒
  Operand.Operand →
  m Operand.Operand
loadIsPrimaryEle e = getIsPrimaryEle e >>= load Type.i1

getPrimaryNode ∷
  ( HasThrow "err" Errors m,
    HasState "blocks" (Map.HashMap Name.Name BlockState) m,
    HasState "count" Word m,
    HasState "currentBlock" Name.Name m
  ) ⇒
  Type.Type →
  Operand.Operand →
  m Operand.Operand
getPrimaryNode nodePtrType bothPrimary =
  getElementPtr $
    Types.Minimal
      { Types.type' = nodePtrType,
        Types.address' = bothPrimary,
        Types.indincies' = Block.constant32List [0, 1]
      }

loadPrimaryNode ∷
  ( HasThrow "err" Errors m,
    HasState "blocks" (Map.HashMap Name.Name BlockState) m,
    HasState "count" Word m,
    HasState "currentBlock" Name.Name m
  ) ⇒
  Type.Type →
  Operand.Operand →
  m Operand.Operand
loadPrimaryNode nodePtrType e = getPrimaryNode nodePtrType e >>= load nodePtrType
