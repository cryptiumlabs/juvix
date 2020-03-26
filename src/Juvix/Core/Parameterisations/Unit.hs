module Juvix.Core.Parameterisations.Unit where

import Juvix.Core.Types hiding
  ( apply,
    parseTy,
    parseVal,
    reservedNames,
    reservedOpNames,
    typeOf,
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

typeOf ∷ Val → NonEmpty Ty
typeOf Val = Ty :| []

apply ∷ Val → Val → Maybe Val
apply _ _ = Nothing

parseTy ∷ Token.GenTokenParser String () Identity → Parser Ty
parseTy lexer = do
  Token.reserved lexer "Unit"
  pure Ty

parseVal ∷ Token.GenTokenParser String () Identity → Parser Val
parseVal lexer = do
  Token.reserved lexer "()"
  pure Val

reservedNames ∷ [String]
reservedNames = ["Unit", "()"]

reservedOpNames ∷ [String]
reservedOpNames = []

t ∷ Parameterisation Ty Val
t =
  Parameterisation typeOf apply parseTy parseVal reservedNames reservedOpNames
