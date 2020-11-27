-- | Shared between Types and Sum
module Juvix.Backends.LLVM.Codegen.Shared where

import           Juvix.Library         hiding (Type)
import qualified Juvix.Library.HashMap as Map
import           LLVM.AST

type SymbolTable = Map.T Symbol Operand

type TypeTable = Map.T Symbol Type

data SumInfo
  = S
      { sum'     :: Symbol,
        offset   :: Int,
        tagSize' :: Word32
      }
  deriving (Show, Eq)

-- | a mapping between the variant and the sum type along with
-- the tag associated with it
type VariantToType = Map.T Symbol SumInfo

type Names = Map.T Symbol Int

uniqueName :: Symbol -> Names -> (Symbol, Names)
uniqueName nm ns =
  case Map.lookup nm ns of
    Nothing -> (nm, Map.insert nm 1 ns)
    Just ix -> (intern (show nm <> show ix), Map.insert nm (succ ix) ns)

instance Hashable Name
