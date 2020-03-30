module Juvix.Backends.ArithmeticCircuit.Parameterisation.Booleans where

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
import qualified Text.Show as Show
import Prelude (String)
import Circuit.Arithmetic (Wire(..))
import Circuit.Expr
import Circuit.Lang

-- k: primitive type: boolean
data Ty
  = Ty
  deriving (Show, Eq)

-- c: primitive constant and f: functions
data Val f
  = Val (Expr Wire f Bool) -- c
  | Or -- f or gate
  | And -- f and gate
  | Curried (Val f) (Expr Wire f Bool)


instance Show f => Show (Val f) where
  show (Val (EConstBool x)) = "Bool " <> show x
  show Or = "||"
  show And = "&&"
  show (Curried val (EConstBool x)) = Juvix.Library.show val <> " " <> Show.show x
  show _ = ""

typeOf ∷ Val f → NonEmpty Ty
typeOf (Val _) = Ty :| []
typeOf Or  = Ty :| [Ty, Ty]
typeOf And = Ty :| [Ty, Ty]
typeOf (Curried _ _) = Ty :| [Ty]

apply ∷ Val f → Val f → Maybe (Val f)
apply Or  (Val x) = pure (Curried Or x)
apply And (Val x) = pure (Curried And x)
apply (Curried And x) (Val y) = pure $ Val (and_ x y)
apply (Curried Or x) (Val y) = pure $ Val (or_ x y)
apply _ _ = Nothing

parseTy ∷ Token.GenTokenParser String () Identity → Parser Ty
parseTy lexer = do
  Token.reserved lexer "Bool"
  pure Ty

parseVal ∷ Token.GenTokenParser String () Identity → Parser (Val f)
parseVal lexer =
  parseTrue lexer <|>
  parseFalse lexer <|>
  parseOr lexer <|>
  parseAnd lexer

parseTrue ∷ Token.GenTokenParser String () Identity → Parser (Val f)
parseTrue lexer = Token.reserved lexer "T" >> pure (Val (EConstBool True))

parseFalse ∷ Token.GenTokenParser String () Identity → Parser (Val f)
parseFalse lexer = Token.reserved lexer "F" >> pure (Val (EConstBool False))

parseOr ∷ Token.GenTokenParser String () Identity → Parser (Val f)
parseOr lexer = Token.reserved lexer "||" >> pure Or

parseAnd ∷ Token.GenTokenParser String () Identity → Parser (Val f)
parseAnd lexer = Token.reserved lexer "&&" >> pure And

reservedNames ∷ [String]
reservedNames = ["Bool", "T", "F", "||", "&&"]

reservedOpNames ∷ [String]
reservedOpNames = []

t ∷ Parameterisation Ty (Val f)
t =
  Parameterisation typeOf apply parseTy parseVal reservedNames reservedOpNames
