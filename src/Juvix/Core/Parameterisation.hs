module Juvix.Core.Parameterisation where

import Juvix.Library
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as Token
import Prelude (String)

data Parameterisation primTy primVal
  = Parameterisation
      { -- Returns an arrow.
        typeOf :: primVal -> NonEmpty primTy,
        apply :: primVal -> primVal -> Maybe primVal,
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

arity :: Parameterisation primTy primVal -> primVal -> Int
arity param = length . typeOf param
