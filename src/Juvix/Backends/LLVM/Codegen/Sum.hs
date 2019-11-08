-- | Provides a mechanism for defining Sum types
module Juvix.Backends.LLVM.Codegen.Sum where

import qualified Juvix.Backends.LLVM.Codegen.Constants as Constants
import Juvix.Backends.LLVM.Codegen.Shared
import Juvix.Library hiding (Type)
import qualified Juvix.Utility.HashMap as Map
import LLVM.AST
import qualified LLVM.AST.Type as Type
import LLVM.AST.Type
import qualified Prelude as Prelude (error)

-----------------------------------------------------------------------------------------
-- Types
-----------------------------------------------------------------------------------------

type Size = Int

-- | Data needed to make a Variant
data VariantInfo
  = Variant
      { size ∷ Size,
        name ∷ Symbol,
        typ' ∷ Type
      }

-----------------------------------------------------------------------------------------
-- Helper functions
-----------------------------------------------------------------------------------------

-- TODO ∷ Optimize this using hacker's delight

-- | 'sumSize' takes a list of variants and creates the array type to hold the
-- largest variant
sumSize ∷ [VariantInfo] → Type
sumSize variants
  -- Assume 8 is the smallest allocation type
  | largest `mod` 16 == 0 = ArrayType (divLarg 16) i16
  | largest `mod` 08 == 0 = ArrayType (divLarg 08) i8
  | otherwise = ArrayType (divLarg 4) Constants.i4
  where
    largest = maximumDef 0 (size <$> variants)
    divLarg = div (fromIntegral largest)

-- TODO ∷ optimize this using bit-wise functions
-- Think hacker's delight
-- Also should we allow smaller than i8?

-- | 'tagSize' takes a list of variants and figures out what the size of the tag should be
tagSize ∷ [VariantInfo] → Type
tagSize variants
  | len < 16 = Constants.i4
  | len < 256 = i8
  | len < 65536 = i16
  | len < 4294967296 = i32
  | otherwise = i64
  where
    len = length variants

-- | grabs the integer length from an IntegerType Type
tagSizeIntExn ∷ Type → Word32
tagSizeIntExn (Type.IntegerType i) = i
tagSizeIntExn _ = Prelude.error "unsupported operation"

createVariantName ∷ Symbol → Symbol → Symbol
createVariantName sumName varName = sumName <> "-" <> varName

-----------------------------------------------------------------------------------------
-- Important functions
-----------------------------------------------------------------------------------------

createSum ∷ [VariantInfo] → Type
createSum variants =
  StructureType
    { isPacked = False,
      elementTypes =
        [tag, arrSize]
    }
  where
    tag = tagSize variants
    arrSize = sumSize variants

-- | updates the type of the variant, to properly have the tag
updateVariant ∷ Type → VariantInfo → VariantInfo
updateVariant tagSize (Variant s n (StructureType p ele)) =
  Variant s n (StructureType p (tagSize : ele))
updateVariant tagSize (Variant s n t) =
  Variant s n (StructureType False [tagSize, t])

-- | 'insertSums' creates a sum type, and inserts the new types into the symbol table
-- and the variant table for all the newly created variants
insertSums ∷
  Symbol →
  [VariantInfo] →
  SymbolTable →
  VariantToType →
  TypeTable →
  (SymbolTable, VariantToType, TypeTable)
insertSums sumName variants symTbl varTbl typTbl = (newSymTbl, newVarTbl, newTypTbl)
  where
    sum' = createSum variants
    tag' = tagSizeIntExn (tagSize variants)
    typTbl' = Map.insert sumName sum' typTbl
    symTbl' =
      Map.insert sumName (LocalReference sum' (mkName (unintern sumName))) symTbl
    newVarTbl =
      fst $
        foldr
          ( \(Variant {name = n}) (tbl, offset) →
              ( Map.insert
                  (createVariantName sumName n)
                  ( S
                      { sum' = sumName,
                        offset = offset,
                        tagSize' = tag'
                      }
                  )
                  tbl,
                succ offset
              )
          )
          (varTbl, 0)
          variants
    newTypTbl =
      foldr
        ( \(Variant _s n t) tbl →
            Map.insert (createVariantName sumName n) t tbl
        )
        typTbl'
        variants
    newSymTbl =
      foldr
        ( \(Variant _s n t) tbl →
            let name = createVariantName sumName n
                operand = LocalReference t (mkName (unintern name))
             in Map.insert name operand tbl
        )
        symTbl'
        variants
