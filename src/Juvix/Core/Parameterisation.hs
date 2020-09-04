module Juvix.Core.Parameterisation where

import Juvix.Library
import Juvix.Library.HashMap (HashMap)
import Juvix.Frontend.Types.Base (NameSymb)
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as Token
import Prelude (String)

-- | @[A, B, ..., Z]@ represents the type
-- @π A -> ρ B -> ... -> Z@ for any usages @π@, @ρ@
type PrimType primTy = NonEmpty primTy

type Builtins p = HashMap NameSymb p

data Parameterisation primTy primVal
  = Parameterisation
      { hasType :: primVal -> PrimType primTy -> Bool,
        arity :: primVal -> Int,
        apply :: primVal -> primVal -> Maybe primVal,

        builtinTypes :: Builtins primTy,
        builtinValues :: Builtins primVal,

        parseTy :: Token.GenTokenParser String () Identity -> Parser primTy,
        parseVal :: Token.GenTokenParser String () Identity -> Parser primVal,
        reservedNames :: [String],
        reservedOpNames :: [String],
        stringTy :: Text -> primTy -> Bool,
        stringVal :: Text -> Maybe primVal,
        intTy :: Integer -> primTy -> Bool,
        intVal :: Integer -> Maybe primVal,
        floatTy :: Double -> primTy -> Bool,
        floatVal :: Double -> Maybe primVal
      }
  deriving (Generic)
