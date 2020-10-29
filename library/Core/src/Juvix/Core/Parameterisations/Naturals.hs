{-# LANGUAGE OverloadedLists #-}

module Juvix.Core.Parameterisations.Naturals where

import qualified Juvix.Core.Parameterisation as P
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

arityT :: Ty -> Natural
arityT _ = 0

applyT :: Ty -> NonEmpty Ty -> Maybe Ty
applyT _ _ = Nothing

arityV :: Val -> Natural
arityV = pred . fromIntegral . length . typeOf

arity :: Val -> Natural
arity = arityV
{-# DEPRECATED arity "use arityV" #-}

applyV1 :: Val -> Val -> Maybe Val
applyV1 Add (Val x) = pure (Curried Add x)
applyV1 Sub (Val x) = pure (Curried Sub x)
applyV1 Mul (Val x) = pure (Curried Mul x)
applyV1 (Curried Add x) (Val y) = pure (Val (x + y))
applyV1 (Curried Sub x) (Val y) = pure (Val (x - y))
applyV1 (Curried Mul x) (Val y) = pure (Val (x * y))
applyV1 _ _ = Nothing

applyV :: Val -> NonEmpty Val -> Maybe Val
applyV f xs = foldlM applyV1 f xs

apply :: Val -> Val -> Maybe Val
apply = applyV1
{-# DEPRECATED apply "use applyV or applyV1" #-}

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
      arityT = \_ -> 0,
      applyT = \_ _ -> Nothing,
      builtinValues,
      arityV,
      applyV,
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
