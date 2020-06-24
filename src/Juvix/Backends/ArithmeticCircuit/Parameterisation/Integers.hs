module Juvix.Backends.ArithmeticCircuit.Parameterisation.Integers where

import qualified Juvix.Backends.ArithmeticCircuit.Parameterisation.FieldElements as FieldElements
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

-- k: primitive type: naturals
data Ty
  = Ty
  deriving (Show, Eq)

class FieldElements.FieldElement e => FInteger (e :: * -> * -> *) i | i -> e where
  prim :: e f i
  add :: e f i -> e f i -> e f i
  mul :: e f i -> e f i -> e f i
  sub :: e f i -> e f i -> e f i
  neg :: e f i -> e f i
  exp :: e f i -> e f i -> e f f
  eq :: e f i -> e f i -> e f g

-- c: primitive constant and f: functions
data Val f i where
  Val :: (FieldElements.FieldElement e, FInteger e i) => e f i -> Val (e f i) i
  Add :: (FieldElements.FieldElement e, FInteger e i) => Val (e f i) i
  Mul :: (FieldElements.FieldElement e, FInteger e i) => Val (e f i) i
  Neg :: (FieldElements.FieldElement e, FInteger e i) => Val (e f i) i
  Exp :: (FieldElements.FieldElement e, FInteger e i) => Val (e f i) i
  Eq :: (FieldElements.FieldElement e, FInteger e i) => Val (e f i) i
  Curried :: (FieldElements.FieldElement e, FInteger e i) => Val (e f i) i -> e f i -> Val (e f b) i

typeOf :: Val f i -> NonEmpty Ty
typeOf (Val _) = Ty :| []
typeOf (Curried _ _) = Ty :| [Ty]
typeOf Add = Ty :| [Ty, Ty]
typeOf Mul = Ty :| [Ty, Ty]

apply :: (FieldElements.FieldElement e, FInteger e i) => Val (e f i) i -> Val (e f i) i -> Maybe (Val (e f i) i)
apply Add (Val x) = pure (Curried Add x)
apply Mul (Val x) = pure (Curried Mul x)
apply (Curried Add x) (Val y) = pure (Val (add x y))
apply (Curried Mul x) (Val y) = pure (Val (mul x y))
apply _ _ = Nothing

parseTy :: Token.GenTokenParser String () Identity -> Parser Ty
parseTy lexer = do
  Token.reserved lexer "Int"
  pure Ty

parseVal :: (FieldElements.FieldElement e, FInteger e i) => Token.GenTokenParser String () Identity -> Parser (Val (e f i) i)
parseVal lexer =
  parseNat lexer <|> parseAdd lexer <|> parseMul lexer

parseNat :: (FieldElements.FieldElement e, FInteger e i) => Token.GenTokenParser String () Identity -> Parser (Val (e f i) i)
parseNat lexer = Val . fromPrim |<< Token.integer lexer
  where
    fromPrim = undefined

parseAdd :: (FieldElements.FieldElement e, FInteger e i) => Token.GenTokenParser String () Identity -> Parser (Val (e f i) i)
parseAdd lexer = Token.reserved lexer "+" >> pure Add

parseMul :: (FieldElements.FieldElement e, FInteger e i) => Token.GenTokenParser String () Identity -> Parser (Val (e f i) i)
parseMul lexer = Token.reserved lexer "*" >> pure Mul

reservedNames :: [String]
reservedNames = ["Int", "+", "-", "*"]

reservedOpNames :: [String]
reservedOpNames = []

t :: (FieldElements.FieldElement e, FInteger e i) => Parameterisation Ty (Val (e f i) i)
t =
  Parameterisation typeOf apply parseTy parseVal reservedNames reservedOpNames
