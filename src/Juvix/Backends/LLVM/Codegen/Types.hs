{-# LANGUAGE ConstraintKinds #-}

module Juvix.Backends.LLVM.Codegen.Types
  ( module Juvix.Backends.LLVM.Codegen.Types,
    module Juvix.Backends.LLVM.Codegen.Shared,
  )
where

import Data.ByteString.Short hiding (empty)
import qualified Juvix.Backends.LLVM.Codegen.Constants as Constants
import Juvix.Backends.LLVM.Codegen.Shared
import Juvix.Backends.LLVM.Codegen.Sum
import Juvix.Library hiding (Type)
import qualified Juvix.Library.HashMap as Map
import LLVM.AST as AST
import LLVM.AST.AddrSpace
import LLVM.AST.DataLayout (DataLayout (..))
import qualified LLVM.AST.Type as Type
import Prelude ((!!))

--------------------------------------------------------------------------------
-- Codegen State
--------------------------------------------------------------------------------

data CodegenState
  = CodegenState
      { -- | Name of the active block to append to
        currentBlock ∷ Name,
        -- | Blocks for function
        blocks ∷ Map.T Name BlockState,
        -- | Function scope symbol table
        symTab ∷ SymbolTable,
        -- | Mapping from symbol to Type
        typTab ∷ TypeTable,
        -- | a mapping from the variants to the sum type
        varTab ∷ VariantToType,
        -- | Count of basic blocks
        blockCount ∷ Int,
        -- | Count of unnamed instructions
        count ∷ Word,
        -- | Name Supply
        names ∷ Names,
        moduleAST ∷ AST.Module
      }
  deriving (Show, Generic)

data BlockState
  = BlockState
      { -- | Block index
        idx ∷ Int,
        -- | Stack of instructions
        stack ∷ [Named Instruction],
        -- | Block terminator
        term ∷ Maybe (Named Terminator)
      }
  deriving (Show, Generic)

data Errors
  = -- | Error when a block does not exist
    NoSuchBlock Text
  | -- | Error when a Variant does not exist
    NoSuchVariant Text
  | -- | Error that should never happen
    DoesNotHappen Text
  | -- | Error that happens when a variable out of scope is called
    VariableNotInScope Text
  | -- | Error that happens when a block lacks a terminator when it should have one
    BlockLackingTerminator Int
  deriving (Show)

newtype Codegen a = CodeGen {runCodegen ∷ ExceptT Errors (State CodegenState) a}
  deriving (Functor, Applicative, Monad)
  deriving
    (HasState "currentBlock" Name)
    via Field "currentBlock" () (MonadState (ExceptT Errors (State CodegenState)))
  deriving
    (HasState "blocks" (Map.T Name BlockState))
    via Field "blocks" () (MonadState (ExceptT Errors (State CodegenState)))
  deriving
    (HasState "symTab" SymbolTable)
    via Field "symTab" () (MonadState (ExceptT Errors (State CodegenState)))
  deriving
    (HasState "varTab" VariantToType)
    via Field "varTab" () (MonadState (ExceptT Errors (State CodegenState)))
  deriving
    (HasState "typTab" TypeTable)
    via Field "typTab" () (MonadState (ExceptT Errors (State CodegenState)))
  deriving
    (HasState "blockCount" Int)
    via Field "blockCount" () (MonadState (ExceptT Errors (State CodegenState)))
  deriving
    (HasState "count" Word)
    via Field "count" () (MonadState (ExceptT Errors (State CodegenState)))
  deriving
    (HasState "names" Names)
    via Field "names" () (MonadState (ExceptT Errors (State CodegenState)))
  deriving
    (HasThrow "err" Errors)
    via MonadError (ExceptT Errors (State CodegenState))
  deriving
    (HasState "moduleAST" AST.Module)
    via Field "moduleAST" () (MonadState (ExceptT Errors (State CodegenState)))

instance HasState "moduleDefinitions" [Definition] Codegen where

  get_ _ = moduleDefinitions <$> (get @"moduleAST")

  put_ _ x = do
    c ← get @"moduleAST"
    put @"moduleAST" (c {moduleDefinitions = x})

  state_ _ state = do
    c ← get @"moduleDefinitions"
    let (a, res) = state c
    put @"moduleDefinitions" res
    pure a

-- TODO ∷ see if this is still useful
newtype LLVM a = LLVM {runLLVM ∷ State AST.Module a}
  deriving (Functor, Applicative, Monad)
  deriving
    (HasState "moduleName" ShortByteString)
    via Field "moduleName" () (MonadState (State AST.Module))
  deriving
    (HasState "moduleSourceFileName" ShortByteString)
    via Field "moduleSourceFileName" () (MonadState (State AST.Module))
  deriving
    (HasState "moduleDataLayout" (Maybe LLVM.AST.DataLayout.DataLayout))
    via Field "moduleDataLayout" () (MonadState (State AST.Module))
  deriving
    (HasState "moduleTargetTriple" (Maybe ShortByteString))
    via Field "moduleTargetTriple" () (MonadState (State AST.Module))
  deriving
    (HasState "moduleDefinitions" [Definition])
    via Field "moduleDefinitions" () (MonadState (State AST.Module))

--------------------------------------------------------------------------------
-- Effect Aliases
--------------------------------------------------------------------------------

type Instruct m =
  ( HasThrow "err" Errors m,
    HasState "blocks" (Map.T Name BlockState) m,
    HasState "currentBlock" Name m
  )

type RetInstruction m =
  ( HasState "count" Word m,
    Instruct m
  )

type MallocNode m =
  ( RetInstruction m,
    HasState "typTab" TypeTable m,
    HasState "varTab" VariantToType m,
    HasState "symTab" SymbolTable m
  )

type NewBlock m =
  ( HasState "blockCount" Int m,
    HasState "blocks" (Map.T Name BlockState) m,
    HasState "names" Names m
  )

type AllocaNode m =
  ( RetInstruction m,
    HasState "typTab" TypeTable m,
    HasState "varTab" VariantToType m
  )

type Define m =
  ( RetInstruction m,
    Externf m,
    HasState "blockCount" Int m,
    HasState "moduleDefinitions" [Definition] m,
    HasState "names" Names m
  )

type External m =
  ( HasState "moduleDefinitions" [Definition] m,
    HasState "symTab" SymbolTable m
  )

type Externf m =
  ( HasState "symTab" SymbolTable m,
    HasThrow "err" Errors m
  )

type Call m =
  ( RetInstruction m,
    HasState "symTab" SymbolTable m
  )

--------------------------------------------------------------------------------
-- Haskell Types
--------------------------------------------------------------------------------

data MinimalPtr
  = Minimal
      { address' ∷ Operand,
        indincies' ∷ [Operand],
        type' ∷ Type
      }
  deriving (Show)

--------------------------------------------------------------------------------
-- LLVM Type Operations
--------------------------------------------------------------------------------

-- TODO :: Replace with safe lens call instead!
intoStructTypeErr ∷ Integral a ⇒ Type → a → Type
intoStructTypeErr typ' i = elementTypes typ' !! fromIntegral i

--------------------------------------------------------------------------------
-- LLVM Types
--------------------------------------------------------------------------------

-- TODO ∷ change all Type → Type into a reader monad
-- instantiate similar to how Parsec does it

-- | 'varientToType' takes the type out of the variant
varientToType ∷ VariantInfo → Type
varientToType = typ'

-- | 'numPortsSmall' is used for the number of ports that fit within 16 bits
numPortsSmall ∷ VariantInfo
numPortsSmall =
  updateVariant
    Type.i1
    Variant
      { size = 32,
        name = "small",
        typ' = numPortsSmallValue
      }

numPortsSmallType ∷ Type
numPortsSmallType = typ' numPortsSmall

pointerOf ∷ Type → Type
pointerOf typ = PointerType typ (AddrSpace 0)

pointerSizeInt ∷ Num p ⇒ p
pointerSizeInt = 64

pointerSize ∷ Type
pointerSize = Type.i64

numPortsSmallValue ∷ Type
numPortsSmallValue = Type.i64

-- | 'numPortsLarge' is used for the number of ports that don't fit within 16 bits
numPortsLarge ∷ VariantInfo
numPortsLarge =
  updateVariant
    Type.i1
    Variant
      { size = 64,
        name = "large",
        typ' = numPortsLargeValuePtr
      }

numPortsLargeType ∷ Type
numPortsLargeType = typ' numPortsLarge

numPortsLargeValue ∷ Type
numPortsLargeValue = Type.i64

numPortsLargeValueInt ∷ Num p ⇒ p
numPortsLargeValueInt = 64

numPortsLargeValuePtr ∷ Type
numPortsLargeValuePtr = pointerOf numPortsLargeValue

numPortsPointer ∷ Type
numPortsPointer = pointerOf numPortsNameRef

numPortsNameRef ∷ Type
numPortsNameRef = Type.NamedTypeReference numPortsName

numPortsName ∷ IsString p ⇒ p
numPortsName = "graph_num_ports"

-- number of ports on a node or the port offset
numPorts ∷ Type
numPorts =
  typ
    { elementTypes =
        let _ : rest = elementTypes typ
         in Type.i1 : rest
    }
  where
    typ = createSum [numPortsLarge, numPortsSmall]

numPortsSize ∷ Num p ⇒ p
numPortsSize = 65

nodePointerSize ∷ Num p ⇒ p
nodePointerSize = 64

portTypeNameRef ∷ Type
portTypeNameRef = Type.NamedTypeReference portTypeName

portTypeName ∷ IsString p ⇒ p
portTypeName = "graph_port"

portType ∷ Type → Type
portType nodePtr = StructureType
  { isPacked = True,
    elementTypes =
      [ nodePtr, -- the pointer to the other node
        numPortsNameRef -- the offset from the base of the node where the port is
      ]
  }

portPointer ∷ Type
portPointer = pointerOf portTypeNameRef

portTypeSize ∷ Num p ⇒ p
portTypeSize = numPortsSize + pointerSizeInt

-- TODO ∷ Figure out how to have an un-tagged union here for all baked in types
dataType ∷ Type
dataType = Constants.int

dataTypeSize ∷ Num p ⇒ p
dataTypeSize = 64

-- | Construct a 32 bit port space so we can put many inside a node cheaply
-- The pointer points to the beginning of a node and an offset
nodePointer ∷ Type
nodePointer = pointerOf nodeTypeNameRef

nodeTypeNameRef ∷ Type
nodeTypeNameRef = Type.NamedTypeReference nodeTypeName

nodeTypeName ∷ IsString p ⇒ p
nodeTypeName = "graph_node"

nodeType ∷ [Type] → Type
nodeType extraData = StructureType
  { isPacked = True,
    elementTypes =
      [ numPortsNameRef, -- length of the portData
        portData, -- variable size array of ports
        dataArray -- variable size array of data the node stores
      ]
        <> extraData -- contains tag and other data a node may need
  }

dataArray ∷ Type
dataArray = pointerOf (ArrayType 0 dataType)

portData ∷ Type
portData = pointerOf (ArrayType 0 portTypeNameRef)

-- TODO ∷ This changes per platform
vaList ∷ Type
vaList = StructureType
  { isPacked = True,
    elementTypes = [pointerOf Type.i8]
  }

bothPrimary ∷ Type → Type
bothPrimary nodePtrType = StructureType
  { isPacked = False,
    elementTypes = [Type.i1, nodePtrType]
  }

voidStarTy ∷ Type
voidStarTy = pointerOf VoidType

voidTy ∷ Type
voidTy = VoidType

size_t ∷ Type
size_t = IntegerType 64

size_t_int ∷ Num p ⇒ p
size_t_int = 64
