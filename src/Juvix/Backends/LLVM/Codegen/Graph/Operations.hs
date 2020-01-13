-- | Functions that help with more complex Graph operations
module Juvix.Backends.LLVM.Codegen.Graph.Operations where

import qualified Juvix.Backends.LLVM.Codegen.Block as Block
import qualified Juvix.Backends.LLVM.Codegen.Types as Types
import Juvix.Library hiding (Type, link, local)
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Name as Name
import qualified LLVM.AST.Operand as Operand
import qualified LLVM.AST.Type as Type

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
  ( Types.Call m,
    HasState "blockCount" Int m,
    HasState "names" Types.Names m
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
  Types.Call m ⇒ Operand.Operand → Operand.Operand → Operand.Operand → m ()
setPortType portPtr node givenOffsetPtr = do
  nodePtr ← Block.getElementPtr $
    Types.Minimal
      { Types.type' = Types.pointerOf Types.nodePointer,
        Types.address' = portPtr,
        Types.indincies' = Block.constant32List [0, 0]
      }
  offsetPtr ← Block.getElementPtr $
    Types.Minimal
      { Types.type' = Types.numPortsPointer,
        Types.address' = portPtr,
        Types.indincies' = Block.constant32List [0, 1]
      }
  -- now store the pointer to the Newport
  Block.store nodePtr node
  -- store the offset
  offset ← Block.load Types.numPortsNameRef givenOffsetPtr
  Block.store offsetPtr offset
  pure ()

-- | 'getPort' takes a node* and a numPort* for operands and gives back a port*
getPort ∷
  ( Types.RetInstruction m,
    HasState "blockCount" Int m,
    HasState "names" Types.Names m
  ) ⇒
  Operand.Operand →
  Operand.Operand →
  m Operand.Operand
getPort node port = do
  portsPtr ← loadPortData node
  intOfNumPorts Types.portPointer port $ \value → do
    Block.getElementPtr $
      Types.Minimal
        { Types.type' = Types.portPointer,
          Types.address' = portsPtr,
          Types.indincies' =
            [ Operand.ConstantOperand (C.Int 32 0),
              value
            ]
        }

-- TODO ∷ make a nicer one without a continuation!
-- | 'intOfNumPorts' generates an int of two different sizes to be used in cont logic
-- the type referees to the final type in the cont logic
intOfNumPorts ∷
  ( Types.RetInstruction m,
    HasState "blockCount" Int m,
    HasState "names" Types.Names m
  ) ⇒
  Type.Type →
  Operand.Operand →
  (Operand.Operand → m Operand.Operand) →
  m Operand.Operand
intOfNumPorts typ numPort cont
  | Types.bitSizeEncodingPoint =
    Block.load (Type.IntegerType Types.addressSpace) numPort >>= cont
  | otherwise = do
    -- grab the tag from the numPort
    tag ← Block.loadElementPtr $
      Types.Minimal
        { Types.type' = Type.i1,
          Types.address' = numPort,
          Types.indincies' = Block.constant32List [0, 0]
        }
    Block.generateIf typ tag largeBranch smallBranch
  where
    smallBranch = branchGen Types.numPortsSmall Types.numPortsSmallValue return

    largeBranch = branchGen Types.numPortsLarge Types.numPortsLargeValuePtr $
      \vPtr →
        Block.loadElementPtr $
          Types.Minimal
            { Types.type' = Types.numPortsLargeValue,
              Types.address' = vPtr,
              Types.indincies' = Block.constant32List [0]
            }

    -- Generic logic
    branchGen variant variantType extraDeref = do
      casted ← Block.bitCast numPort (Types.pointerOf (Types.varientToType variant))
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
  ( Types.RetInstruction m,
    HasState "blockCount" Int m,
    HasState "names" Types.Names m
  ) ⇒
  Operand.Operand →
  m Operand.Operand
portPointsTo portType = do
  -- TODO ∷ see if there is any special logic for packed Types
  -- Do I have to index by size?
  nodePtr ← Block.loadElementPtr $
    Types.Minimal
      { Types.type' = Types.nodePointer,
        Types.address' = portType,
        Types.indincies' = Block.constant32List [0, 0]
      }
  -- Get the numPort
  numPort ← Block.getElementPtr $
    Types.Minimal
      { Types.type' = Types.numPortsPointer,
        Types.address' = portType,
        -- Index may change due to being packed
        Types.indincies' = Block.constant32List [0, 1]
      }
  getPort nodePtr numPort

-- | Allocates a 'numPorts'
createNumPortsStaticGen ∷
  ( Types.RetInstruction m,
    HasState "typTab" Types.TypeTable m,
    HasState "varTab" Types.VariantToType m
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
          ptr ← alloc Types.nodePointer Types.nodePointerSize
          Block.store ptr value
          large ← allocVar "numPorts_large" [ptr] Types.numPortsSize
          castIt large

-- | Allocates 'numPorts' via allcoca
allocaNumPortsStatic ∷
  ( Types.RetInstruction m,
    HasState "typTab" Types.TypeTable m,
    HasState "varTab" Types.VariantToType m
  ) ⇒
  Bool →
  Operand.Operand →
  m Operand.Operand
allocaNumPortsStatic isLarge value =
  createNumPortsStaticGen
    isLarge
    value
    (\s xs _ → Block.allocaVariant s xs)
    (const . Block.alloca)

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
    (flip Block.malloc . Types.pointerOf)

createNumPortNumGen ∷ Integer → (Bool → Operand.Operand → p) → p
createNumPortNumGen n alloc
  | n <= 2 ^ (Types.numPortsSize - 1 ∷ Integer) =
    alloc False (Operand.ConstantOperand (C.Int Types.pointerSizeInt n))
  | otherwise =
    alloc True (Operand.ConstantOperand (C.Int Types.numPortsLargeValueInt n))

-- | like 'allocaNumPortStatic', except it takes a number and allocates the correct operand
allocaNumPortNum ∷
  Types.AllocaNode m ⇒ Integer → m Operand.Operand
allocaNumPortNum n
  | Types.bitSizeEncodingPoint = Block.alloca (Type.IntegerType Types.addressSpace)
  | otherwise = createNumPortNumGen n allocaNumPortsStatic

-- | like 'mallocNumPortStatic', except it takes a number and allocates the correct operand
mallocNumPortNum ∷
  Types.MallocNode m ⇒ Integer → m Operand.Operand
mallocNumPortNum n
  | Types.bitSizeEncodingPoint =
    Block.malloc
      Types.numPortsSize
      (Types.pointerOf (Type.IntegerType Types.addressSpace))
  | otherwise = createNumPortNumGen n mallocNumPortsStatic

--------------------------------------------------------------------------------
-- Type aliases accessor functions
--------------------------------------------------------------------------------
getPortData ∷ Types.RetInstruction m ⇒ Operand.Operand → m Operand.Operand
getPortData nodePtr =
  Block.getElementPtr $
    Types.Minimal
      { Types.type' = Types.pointerOf Types.portData,
        Types.address' = nodePtr,
        Types.indincies' = Block.constant32List [0, loc]
      }
  where
    loc
      | Types.bitSizeEncodingPoint = 1
      | otherwise = 2

loadPortData ∷ Types.RetInstruction m ⇒ Operand.Operand → m Operand.Operand
loadPortData np = getPortData np >>= Block.load Types.portData

getDataArray ∷ Types.RetInstruction m ⇒ Operand.Operand → m Operand.Operand
getDataArray nodePtr =
  Block.getElementPtr $
    Types.Minimal
      { Types.type' = Types.pointerOf Types.dataArray,
        Types.address' = nodePtr,
        Types.indincies' = Block.constant32List [0, loc]
      }
  where
    loc
      | Types.bitSizeEncodingPoint = 2
      | otherwise = 3

loadDataArray ∷ Types.RetInstruction m ⇒ Operand.Operand → m Operand.Operand
loadDataArray np = getDataArray np >>= Block.load Types.dataArray

--------------------------------------------------------------------------------
-- Accessor aliases
--------------------------------------------------------------------------------

getIsPrimaryEle ∷ Types.RetInstruction m ⇒ Operand.Operand → m Operand.Operand
getIsPrimaryEle bothPrimary =
  Block.getElementPtr $
    Types.Minimal
      { Types.type' = Types.pointerOf Type.i1,
        Types.address' = bothPrimary,
        Types.indincies' = Block.constant32List [0, 0]
      }

loadIsPrimaryEle ∷ Types.RetInstruction m ⇒ Operand.Operand → m Operand.Operand
loadIsPrimaryEle e = getIsPrimaryEle e >>= Block.load Type.i1

getPrimaryNode ∷ Types.RetInstruction m ⇒ Type.Type → Operand.Operand → m Operand.Operand
getPrimaryNode nodePtrType bothPrimary =
  Block.getElementPtr $
    Types.Minimal
      { Types.type' = Types.pointerOf nodePtrType,
        Types.address' = bothPrimary,
        Types.indincies' = Block.constant32List [0, 1]
      }

loadPrimaryNode ∷ Types.RetInstruction m ⇒ Type.Type → Operand.Operand → m Operand.Operand
loadPrimaryNode nodePtrType e = getPrimaryNode nodePtrType e >>= Block.load nodePtrType
