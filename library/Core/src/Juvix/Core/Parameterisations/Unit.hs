{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedLists #-}

module Juvix.Core.Parameterisations.Unit where

import qualified Juvix.Core.Parameterisation as P
import Juvix.Library hiding ((<|>))
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as Token
import Prelude (String)

-- k: primitive type: unit
data Ty
  = Ty
  deriving (Show, Eq)

-- c: primitive constant and f: functions
data Val
  = Val
  deriving (Show, Eq)

hasType :: Val -> P.PrimType Ty -> Bool
hasType Val (Ty :| []) = True
hasType _ _ = False

instance P.CanApply Val where
  arity _ = 0
  apply f xs = Left $ P.ExtraArguments f xs

instance P.CanApply (P.TypedPrim Ty Val) where
  arity _ = 0
  apply f xs = Left $ P.ExtraArguments f xs

parseTy :: Token.GenTokenParser String () Identity -> Parser Ty
parseTy lexer = do
  Token.reserved lexer "Unit"
  pure Ty

parseVal :: Token.GenTokenParser String () Identity -> Parser Val
parseVal lexer = do
  Token.reserved lexer "tt"
  pure Val

reservedNames :: [String]
reservedNames = ["Unit", "tt"]

reservedOpNames :: [String]
reservedOpNames = []

builtinTypes :: P.Builtins Ty
builtinTypes = [(["Unit"], Ty)]

builtinValues :: P.Builtins Val
builtinValues = [(["tt"], Val)]

t :: P.Parameterisation Ty Val
t =
  P.Parameterisation
    { hasType,
      builtinTypes,
      builtinValues,
      parseTy,
      parseVal,
      reservedNames,
      reservedOpNames,
      stringTy = \_ _ -> False,
      stringVal = const Nothing,
      intTy = \_ _ -> False,
      intVal = const Nothing,
      floatTy = \_ _ -> False,
      floatVal = const Nothing
    }
