module Juvix.Backends.ArithmeticCircuit.Parameterisation.Booleans where

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

-- k: primitive type: boolean
data Ty = Ty
  deriving (Show, Eq)

-- `b` is whatever datatype we decide to use for booleans
-- and it is uniquely defined by `e`, which is the whatever
-- definition we decide to go for field elements.
class FieldElements.FieldElement e => Boolean (e :: * -> * -> *) b | b -> e where
  true :: e f b
  false :: e f b
  and' :: e f b -> e f b -> e f b
  or' :: e f b -> e f b -> e f b
  not' :: e f b -> e f b

type FieldT e b = (FieldElements.FieldElement e, Boolean e b)

-- c: primitive constant and f: functions
data Val f b where
  Val :: FieldT e b => e f b -> Val (e f b) b
  Or :: FieldT e b => Val (e f b) b
  And :: FieldT e b => Val (e f b) b
  Not :: FieldT e b => Val (e f b) b
  Curried :: FieldT e b => Val (e f b) b -> e f b -> Val (e f b) b

typeOf :: Val f b -> NonEmpty Ty
typeOf (Val _) = Ty :| []
typeOf Or = Ty :| [Ty, Ty]
typeOf And = Ty :| [Ty, Ty]
typeOf Not = Ty :| [Ty, Ty]
typeOf (Curried _ _) = Ty :| [Ty]

apply :: FieldT e b => Val (e f b) b -> Val (e f b) b -> Maybe (Val (e f b) b)
apply Or (Val x) = pure $ Curried Or x
apply And (Val x) = pure $ Curried And x
apply Not (Val x) = pure $ Val (not' x)
apply (Curried And x) (Val y) = pure $ Val (and' x y)
apply (Curried Or x) (Val y) = pure $ Val (or' x y)
apply _ _ = Nothing

parseTy :: Token.GenTokenParser String () Identity -> Parser Ty
parseTy lexer = do
  Token.reserved lexer "Bool"
  pure Ty

parseVal :: FieldT e b => Token.TokenParser () -> Parser (Val (e f b) b)
parseVal lexer =
  parseTrue lexer
    <|> parseFalse lexer
    <|> parseOr lexer
    <|> parseAnd lexer

parseTrue :: FieldT e b => Token.TokenParser () -> Parser (Val (e f b) b)
parseTrue lexer = Token.reserved lexer "T" >> pure (Val true)

parseFalse :: FieldT e b => Token.TokenParser () -> Parser (Val (e f b) b)
parseFalse lexer = Token.reserved lexer "F" >> pure (Val false)

parseOr :: FieldT e b => Token.TokenParser () -> Parser (Val (e f b) b)
parseOr lexer = Token.reserved lexer "||" >> pure Or

parseAnd :: FieldT e b => Token.TokenParser () -> Parser (Val (e f b) b)
parseAnd lexer = Token.reserved lexer "&&" >> pure And

reservedNames :: [String]
reservedNames = ["Bool", "T", "F", "||", "&&"]

reservedOpNames :: [String]
reservedOpNames = []

t :: FieldT e b => Parameterisation Ty (Val (e f b) b)
t = Parameterisation typeOf apply parseTy parseVal reservedNames reservedOpNames
