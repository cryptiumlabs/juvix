module Juvix.Core.Parameterisations.Naturals where

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
import Juvix.Library hiding ((<|>), natVal)
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as Token
import Text.Show
import Prelude (String)

-- k: primitive type: naturals
data Ty
  = Ty
  deriving (Show, Eq)

-- c: primitive constant and f: functions
data Val
  = Val Natural -- c
  | Add -- f addition
  | Sub -- f subtraction
  | Mul -- f multiplication
  | Curried Val Natural
  deriving (Eq)

instance Show Val where
  show (Val x) = "Nat " <> Text.Show.show x
  show Add = "add"
  show Sub = "sub"
  show Mul = "mul"
  show (Curried x y) = Juvix.Library.show x <> " " <> Text.Show.show y

typeOf :: Val -> PrimType Ty
typeOf (Val _) = Ty :| []
typeOf (Curried _ _) = Ty :| [Ty]
typeOf Add = Ty :| [Ty, Ty]
typeOf Sub = Ty :| [Ty, Ty]
typeOf Mul = Ty :| [Ty, Ty]

hasType :: Val -> PrimType Ty -> Bool
hasType x ty = ty == typeOf x where

arity :: Val -> Int
arity = pred . length . typeOf

apply :: Val -> Val -> Maybe Val
apply Add (Val x) = pure (Curried Add x)
apply Sub (Val x) = pure (Curried Sub x)
apply Mul (Val x) = pure (Curried Mul x)
apply (Curried Add x) (Val y) = pure (Val (x + y))
apply (Curried Sub x) (Val y) = pure (Val (x - y))
apply (Curried Mul x) (Val y) = pure (Val (x * y))
apply _ _ = Nothing

parseTy :: Token.GenTokenParser String () Identity -> Parser Ty
parseTy lexer = do
  Token.reserved lexer "Nat"
  pure Ty

parseVal :: Token.GenTokenParser String () Identity -> Parser Val
parseVal lexer =
  parseNat lexer <|> parseAdd lexer <|> parseSub lexer <|> parseMul lexer

parseNat :: Token.GenTokenParser String () Identity -> Parser Val
parseNat lexer = Val . fromIntegral |<< Token.natural lexer

parseAdd :: Token.GenTokenParser String () Identity -> Parser Val
parseAdd lexer = Token.reserved lexer "add" >> pure Add

parseSub :: Token.GenTokenParser String () Identity -> Parser Val
parseSub lexer = Token.reserved lexer "sub" >> pure Sub

parseMul :: Token.GenTokenParser String () Identity -> Parser Val
parseMul lexer = Token.reserved lexer "mul" >> pure Mul

reservedNames :: [String]
reservedNames = ["Nat", "add", "sub", "mul"]

reservedOpNames :: [String]
reservedOpNames = []

isNat :: Integer -> Bool
isNat i = i >= 0

natVal :: Integer -> Maybe Val
natVal i = if i >= 0 then Just (Val (fromIntegral i)) else Nothing

t :: Parameterisation Ty Val
t =
  Parameterisation {
    hasType, arity, apply, parseTy, parseVal, reservedNames, reservedOpNames,
    stringTy = \_ _ -> False,
    stringVal = const Nothing,
    intTy = \i _ -> isNat i,
    intVal = natVal,
    floatTy = \_ _ -> False,
    floatVal = const Nothing
  }
