module Juvix.Core.Parameterisation where

import Juvix.Library
import Juvix.Library.HashMap (HashMap)
import qualified Juvix.Library.NameSymbol as NameSymbol
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as Token
import Prelude (String)

-- | @[A, B, ..., Z]@ represents the type
-- @π A -> ρ B -> ... -> Z@ for any usages @π@, @ρ@
type PrimType primTy = NonEmpty primTy

type Builtins p = HashMap NameSymbol.T p

data Parameterisation primTy primVal
  = Parameterisation
      { hasType :: primVal -> PrimType primTy -> Bool,
        builtinTypes :: Builtins primTy,
        arityT :: primTy -> Natural,
        -- | 'Nothing' if wrong arity
        applyT :: primTy -> NonEmpty primTy -> Maybe primTy,

        builtinValues :: Builtins primVal,
        arityV :: primVal -> Natural,
        -- | 'Nothing' if wrong arity or type
        applyV :: primVal -> NonEmpty primVal -> Maybe primVal,

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

arity :: Parameterisation primTy primVal -> primVal -> Natural
arity = arityV
{-# DEPRECATED arity "use arityV" #-}

apply :: Parameterisation primTy primVal -> primVal -> primVal -> Maybe primVal
apply p x y = applyV p x (y :| [])
{-# DEPRECATED apply "use applyV" #-}

{-# DEPRECATED
    parseTy, parseVal, reservedNames, reservedOpNames
    "TODO: update parser to not use these"
  #-}
