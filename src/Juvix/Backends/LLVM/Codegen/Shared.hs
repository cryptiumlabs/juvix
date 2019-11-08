-- | Shared between Types and Sum
module Juvix.Backends.LLVM.Codegen.Shared where

import Juvix.Library hiding (Type)
import qualified Juvix.Library.HashMap as Map
import LLVM.AST
import qualified LLVM.AST as AST ()
import qualified LLVM.AST.Constant as C ()
import LLVM.AST.Global as Global ()

type SymbolTable = Map.Map Symbol Operand

type TypeTable = Map.Map Symbol Type

data SumInfo
  = S
      { sum' ∷ Symbol,
        offset ∷ Int,
        tagSize' ∷ Word32
      }
  deriving (Show, Eq)

-- | a mapping between the variant and the sum type along with
-- the tag associated with it
type VariantToType = Map.Map Symbol SumInfo

type Names = Map.Map Symbol Int

uniqueName ∷ Symbol → Names → (Symbol, Names)
uniqueName nm ns =
  case Map.lookup nm ns of
    Nothing → (nm, Map.insert nm 1 ns)
    Just ix → (intern (show nm <> show ix), Map.insert nm (succ ix) ns)

instance Hashable Name
