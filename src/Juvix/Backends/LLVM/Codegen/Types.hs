module Juvix.Backends.LLVM.Codegen.Types
  ( module Juvix.Backends.LLVM.Codegen.Types,
    module Juvix.Backends.LLVM.Codegen.Shared,
  )
where

import Data.ByteString.Short
import qualified Juvix.Backends.LLVM.Codegen.Constants as Constants
import Juvix.Backends.LLVM.Codegen.Shared
import Juvix.Backends.LLVM.Codegen.Sum
import Juvix.Library hiding (Type)
import qualified Juvix.Library.HashMap as Map
import LLVM.AST as AST
import LLVM.AST.AddrSpace
import LLVM.AST.DataLayout (DataLayout (..))
import qualified LLVM.AST.Type as Type

--------------------------------------------------------------------------------
-- Codegen State
--------------------------------------------------------------------------------

data CodegenState
  = CodegenState
      { -- | Name of the active block to append to
        currentBlock ∷ Name,
        -- | Blocks for function
        blocks ∷ Map.Map Name BlockState,
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
        names ∷ Names
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
  deriving (Show)

newtype Codegen a = CodeGen {runCodegen ∷ ExceptT Errors (State CodegenState) a}
  deriving (Functor, Applicative, Monad)
  deriving
    (HasState "currentBlock" Name)
    via Field "currentBlock" () (MonadState (ExceptT Errors (State CodegenState)))
  deriving
    (HasState "blocks" (Map.Map Name BlockState))
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
-- LLVM Types
--------------------------------------------------------------------------------

-- | 'varientToType' takes the type out of the variant
varientToType ∷ VariantInfo → Type
varientToType = typ'

-- | 'numPortsSmall' is used for the number of ports that fit within 16 bits
numPortsSmall ∷ VariantInfo
numPortsSmall =
  updateVariant
    Type.i1
    Variant
      { size = 16,
        name = "small",
        typ' = numPortsSmallValue
      }

pointerSize ∷ Type
pointerSize = Type.i16

numPortsSmallValue ∷ Type
numPortsSmallValue = Type.i16

-- | 'numPortsLarge' is used for the number of ports that don't fit within 16 bits
numPortsLarge ∷ VariantInfo
numPortsLarge =
  updateVariant
    Type.i1
    Variant
      { size = 16,
        name = "large",
        typ' = numPortsLargeValuePtr
      }

numPortsLargeValue ∷ Type
numPortsLargeValue = Type.i64

numPortsLargeValuePtr ∷ Type
numPortsLargeValuePtr = PointerType
  { -- TODO ∷ change to something more variable than i64
    pointerReferent = numPortsLargeValue,
    pointerAddrSpace = AddrSpace 16
  }

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
numPortsSize = 17

-- | Construct a 16 bit port space so we can put many inside a node cheaply
-- The pointer points to the beginning of a node and an offset
nodePointer ∷ Type
nodePointer = PointerType
  { pointerReferent = nodeType,
    pointerAddrSpace = AddrSpace 16
  }

portType ∷ Type
portType = StructureType
  { isPacked = True,
    elementTypes =
      [ nodePointer, -- the pointer to the other node
        numPorts -- the offset from the base of the node where the port is
      ]
  }

-- TODO ∷ getElementPtr returns int32*?
portPointer ∷ Type
portPointer = PointerType
  { pointerReferent = portType,
    pointerAddrSpace = AddrSpace 32
  }

portTypeSize ∷ Num p ⇒ p
portTypeSize = 33

-- TODO ∷ Figure out how to have an un-tagged union here for all baked in types
dataType ∷ Type
dataType = Constants.int

dataTypeSize ∷ Num p ⇒ p
dataTypeSize = 64

-- TODO ∷ Figure out how to get varying data in here
nodeType ∷ Type
nodeType = StructureType
  { isPacked = True,
    elementTypes =
      [ numPorts, -- length of the portData
        ArrayType 0 portType, -- variable size array of ports
        ArrayType 0 dataType -- variable size array of data the node stores
      ]
  }

portData ∷ Type
portData = ArrayType 0 portType

-- Holds the port type and the size of it for easy transition into nodeType
portArrayLen ∷ Type
portArrayLen = StructureType
  { isPacked = False,
    elementTypes =
      [ numPorts,
        portData
      ]
  }
