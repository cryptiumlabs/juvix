-- | Operations necessary to update nodes
module Juvix.Backends.LLVM.Codegen.Graph where

import Juvix.Backends.LLVM.Codegen.Block as Block
import Juvix.Backends.LLVM.Codegen.Types as Types
import Juvix.Library hiding (Type, local)
import qualified Juvix.Library.HashMap as Map
import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Name as Name
import qualified LLVM.AST.Operand as Operand
import qualified LLVM.AST.Type as Type

-- TODO ∷ abstract out getElementPointer and load into a single operation

--------------------------------------------------------------------------------
-- Main Functions
--------------------------------------------------------------------------------

-- take 4 Operand.Operand
link ∷
  ( HasThrow "err" Errors m,
    HasState "blockCount" Int m,
    HasState "blocks" (Map.HashMap Name.Name BlockState) m,
    HasState "count" Word m,
    HasState "currentBlock" Name.Name m,
    HasState "moduleDefinitions" [AST.Definition] m,
    HasState "names" Names m,
    HasState "symtab" (Map.HashMap Symbol Operand.Operand) m
  ) ⇒
  m Operand.Operand
link = body >>= define Type.void "link" args
  where
    args =
      ( [ (nodeType, "node_1"),
          (numPorts, "port_1"),
          (nodeType, "node_2"),
          (numPorts, "port_2")
        ] ∷
          [(Type.Type, Name.Name)]
      )
    -- TODO ∷ Abstract most of the logic in this function
    body = do
      makeFunction "link" args
      setPort ("node_1", "port_1") ("node_2", "port_2")
      setPort ("node_2", "port_2") ("node_1", "port_1")
      _ ← retNull
      createBlocks

-- perform offsets

isBothPrimary = body >>= define Type.i1 "is_both_primary" args
  where
    args = [(nodeType, "node")]
    body = do
      makeFunction "is_both_primary" args
      mainPort ← allocaNumPorts False (Operand.ConstantOperand (C.Int 32 0))
      -- TODO ∷ Maybe bad, we should have an environemnt of edges that I can just call
      edge ← findEdge
      port ← call portType edge (Block.emptyArgs [mainPort])
      nodePtr ← getElementPtr $
        Types.Minimal
          { Types.type' = nodePointer,
            Types.address' = port,
            Types.indincies' = Block.constant32List [0, 1]
          }
      node ← load nodePointer nodePtr
      -- run compare function
      cmp ← undefined
      _ ← ret cmp
      createBlocks

-- The logic assumes that the operation always succeeds
findEdge ∷
  ( HasThrow "err" Errors m,
    HasState "blockCount" Int m,
    HasState "blocks" (Map.HashMap Name.Name BlockState) m,
    HasState "count" Word m,
    HasState "currentBlock" Name.Name m,
    HasState "moduleDefinitions" [AST.Definition] m,
    HasState "names" Names m,
    HasState "symtab" (Map.HashMap Symbol Operand.Operand) m
  ) ⇒
  m Operand.Operand
findEdge = body >>= define portType "find_edge" args
  where
    args = [(nodeType, "node"), (numPorts, "port")]
    body = do
      makeFunction "find_edge" args
      node ← Block.externf "node"
      pNum ← Block.externf "port"
      port ← getPort node pNum
      other ← portPointsTo port
      _ ← ret other
      createBlocks

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

makeFunction ∷
  ( HasThrow "err" Errors m,
    HasState "blockCount" Int m,
    HasState "blocks" (Map.HashMap Name.Name BlockState) m,
    HasState "count" Word m,
    HasState "currentBlock" Name.Name m,
    HasState "names" Names m,
    HasState "symtab" (Map.HashMap Symbol Operand.Operand) m
  ) ⇒
  Symbol →
  [(Type.Type, Name.Name)] →
  m ()
makeFunction name args = do
  entry ← addBlock name
  _ ← setBlock entry
  -- Maybe not needed?
  traverse_
    ( \(typ, nam) → do
        var ← alloca typ
        store var (local typ nam)
        assign (nameToSymbol nam) var
    )
    args

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
  m ()
setPort (n1, p1) (n2, p2) = do
  (no1, po1) ← (,) <$> Block.externf n1 <*> Block.externf p1
  (no2, po2) ← (,) <$> Block.externf n2 <*> Block.externf p2
  -- Grab the int value of port 1
  portLocation ← getPort no1 po1
  p2Ptr ← newPortType no2 po2
  store portLocation p2Ptr
  pure ()

newPortType ∷
  ( HasThrow "err" Errors m,
    HasState "blocks" (Map.HashMap Name.Name BlockState) m,
    HasState "count" Word m,
    HasState "currentBlock" Name.Name m
  ) ⇒
  Operand.Operand →
  Operand.Operand →
  m Operand.Operand
newPortType node offset = do
  newPort ← alloca portType
  -- This is a ptr to a ptr
  nodePtr ← getElementPtr $
    Types.Minimal
      { Types.type' = nodePointer,
        Types.address' = newPort,
        Types.indincies' = Block.constant32List [0, 1]
      }
  offsetPtr ← getElementPtr $
    Types.Minimal
      { Types.type' = numPorts,
        Types.address' = newPort,
        Types.indincies' = Block.constant32List [0, 2]
      }
  -- allocate pointer to the node
  givenNodePtr ← alloca nodePointer
  placeToStoreNode ← getElementPtr $
    Types.Minimal
      { Types.type' = nodeType,
        Types.address' = givenNodePtr,
        Types.indincies' = Block.constant32List [0, 1]
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
  m Operand.Operand
getPort node port = do
  intOfNumPorts portType port $ \value → do
    portsPtr ← getElementPtr $
      Types.Minimal
        { Types.type' = portData,
          Types.address' = node,
          Types.indincies' = Block.constant32List [0, 2]
        }
    ports ← load portData portsPtr
    -- allocate the new pointer
    getElementPtr $
      Types.Minimal
        { Types.type' = portType,
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
  tagPtr ← Block.getElementPtr $
    Types.Minimal
      { Types.type' = Type.i1,
        Types.address' = numPort,
        Types.indincies' = Block.constant32List [0, 1]
      }
  tag ← load Type.i1 tagPtr
  generateIf typ tag smallBranch largeBranch
  where
    smallBranch = branchGen numPortsSmall numPortsSmallValue return

    largeBranch = branchGen numPortsLarge numPortsLargeValuePtr $
      \vPtr → do
        deref2 ← Block.getElementPtr $
          Types.Minimal
            { Types.type' = numPortsLargeValue,
              Types.address' = vPtr,
              Types.indincies' = Block.constant32List [0, 1]
            }
        load numPortsLargeValue deref2

    -- Generic logic
    branchGen variant variantType extraDeref = do
      casted ← bitCast numPort (varientToType variant)
      valueP ← Block.getElementPtr $
        Types.Minimal
          { Types.type' = variantType,
            Types.address' = casted,
            Types.indincies' = Block.constant32List [0, 1]
          }
      value ← load variantType valueP
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
  m Operand.Operand
portPointsTo (portType ∷ Operand.Operand) = do
  -- TODO ∷ see if there is any special logic for packed Types
  -- Do I have to index by size?
  nodePtrPtr ← Block.getElementPtr $
    Types.Minimal
      { Types.type' = nodePointer,
        Types.address' = portType,
        Types.indincies' = Block.constant32List [0, 1]
      }
  -- Finish grabbing the node from the pointer
  nodePtr ← load nodePointer nodePtrPtr
  -- get the node which it points to
  nodeTypePtr ← getElementPtr $
    Types.Minimal
      { Types.type' = nodeType,
        Types.address' = nodePtr,
        Types.indincies' = Block.constant32List [0, 1]
      }
  node ← load nodeType nodeTypePtr
  -- Get the numPort
  numPortPtr ← Block.getElementPtr $
    Types.Minimal
      { Types.type' = numPorts,
        Types.address' = portType,
        -- Index may change due to being packed
        Types.indincies' = Block.constant32List [0, 2]
      }
  numPort ← load numPorts numPortPtr
  getPort node numPort

-- | Allocates a 'numPorts'
allocaNumPorts ∷
  ( HasThrow "err" Errors m,
    HasState "blocks" (Map.HashMap Name.Name BlockState) m,
    HasState "count" Word m,
    HasState "currentBlock" Name.Name m,
    HasState "typTab" TypeTable m,
    HasState "varTab" VariantToType m
  ) ⇒
  Bool →
  Operand.Operand →
  m Operand.Operand
allocaNumPorts (isLarge ∷ Bool) (value ∷ Operand.Operand) =
  -- Call Block.createVariant
  -- Issue is that I need to register this sum type in the map
  -- else it is an error.
  -- see if this is okay, if not make custom logic just for the
  -- sums to create the language
  case isLarge of
    False →
      -- TODO ∷ register this variant
      Block.createVariant "numPorts_large" [value]
    True → do
      -- Allocate a pointer to the value
      ptr ← alloca nodePointer
      store ptr value
      -- TODO ∷ register this variant
      Block.createVariant "numPorts_small" [ptr]
