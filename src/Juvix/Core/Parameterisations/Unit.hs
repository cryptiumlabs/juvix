module Juvix.Core.Parameterisations.Unit where

import qualified Juvix.Core.Parameterisation as P
import Juvix.Core.Types hiding
  ( apply,
    parseTy,
    parseVal,
    reservedNames,
    reservedOpNames,
    hasType,
    arity,
  )
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

hasType :: Val -> PrimType Ty -> Bool
hasType Val (Ty :| []) = True
hasType _ _ = False

arity :: Val -> Int
arity Val = 1

apply :: Val -> Val -> Maybe Val
apply _ _ = Nothing

parseTy :: Token.GenTokenParser String () Identity -> Parser Ty
parseTy lexer = do
  Token.reserved lexer "Unit"
  pure Ty

parseVal :: Token.GenTokenParser String () Identity -> Parser Val
parseVal lexer = do
  Token.reserved lexer "()"
  pure Val

reservedNames :: [String]
reservedNames = ["Unit", "()"]

reservedOpNames :: [String]
reservedOpNames = []

t :: Parameterisation Ty Val
t =
  Parameterisation {
    hasType, arity, apply, parseTy, parseVal, reservedNames, reservedOpNames,
    stringTy = \_ _ -> False,
    stringVal = const Nothing,
    intTy = \_ _ -> False,
    intVal = const Nothing,
    floatTy = \_ _ -> False,
    floatVal = const Nothing
  }
