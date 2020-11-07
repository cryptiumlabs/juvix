{-# LANGUAGE OverloadedLists #-}

module Juvix.Core.Parameterisations.Naturals where

import qualified Juvix.Core.Parameterisation as P
import qualified Juvix.Core.IR.Typechecker.Types as Typed
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

typeOf :: Val -> P.PrimType Ty
typeOf (Val _) = Ty :| []
typeOf (Curried _ _) = Ty :| [Ty]
typeOf Add = Ty :| [Ty, Ty]
typeOf Sub = Ty :| [Ty, Ty]
typeOf Mul = Ty :| [Ty, Ty]

hasType :: Val -> P.PrimType Ty -> Bool
hasType x ty = ty == typeOf x

instance P.CanApply Val where
  arity = pred . fromIntegral . length . typeOf
  apply f xs = app f $ toList xs where
    app Add (Val x : xs) = app (Curried Add x) xs
    app Sub (Val x : xs) = app (Curried Sub x) xs
    app Mul (Val x : xs) = app (Curried Mul x) xs
    app (Curried Add x) (Val y : ys) = app (Val (x + y)) ys
    app (Curried Sub x) (Val y : ys) = app (Val (x - y)) ys
    app (Curried Mul x) (Val y : ys) = app (Val (x * y)) ys
    app n [] = Right n
    app f (x : xs) = Left $ P.ExtraArguments f (x :| xs)

instance P.CanApply (Typed.TypedPrim Ty Val) where


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

builtinTypes :: P.Builtins Ty
builtinTypes = [(["Nat"], Ty)]

builtinValues :: P.Builtins Val
builtinValues = [(["add"], Add), (["sub"], Sub), (["mul"], Mul)]

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
      intTy = \i _ -> isNat i,
      intVal = natVal,
      floatTy = \_ _ -> False,
      floatVal = const Nothing
    }
