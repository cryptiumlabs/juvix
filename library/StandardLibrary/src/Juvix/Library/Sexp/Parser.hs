{-# LANGUAGE TypeApplications #-}

module Juvix.Library.Sexp.Parser (parse) where

import Data.Char (isDigit)
import qualified Data.Text as T
import Juvix.Library hiding (list)
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.Parser as J
import Juvix.Library.Parser.Internal (Parser, ParserError)
import qualified Juvix.Library.Parser.Token as Token
import qualified Juvix.Library.Sexp.Types as Sexp
import qualified Text.Megaparsec as P
import Prelude (fail)

-- | @parse@ parses any sexp expression into the Sexp type
parse :: T.Text -> Either ParserError Sexp.T
parse = P.parse (J.spaceLiner sexp) ""

--------------------------------------------------------------------------------
-- Sexp Main Parsers
--------------------------------------------------------------------------------
sexp :: Parser Sexp.T
sexp = J.spaceLiner (list <|> (Sexp.Atom <$> atom))

list :: Parser Sexp.T
list = do
  d <- parens (many sexp)
  case d of
    [] -> pure Sexp.Nil
    _ -> pure (foldr Sexp.Cons Sexp.Nil d)

atom :: Parser Sexp.Atom
atom = number <|> name

name :: Parser Sexp.Atom
name = do
  sym <- symbol
  pure (Sexp.A sym Nothing)

number :: Parser Sexp.Atom
number = do
  int <- integer
  pure (Sexp.N int Nothing)

symbol :: Parser NameSymbol.T
symbol = do
  s <-
    P.takeWhile1P
      (Just "Valid symbol")
      ( \x ->
          Token.validStartSymbol x
            || Token.validMiddleSymbol x
            || Token.validInfixSymbol x
      )
  internText s |> NameSymbol.fromSymbol |> pure

-- Code stolen from the other parser ☹

integer :: Parser Integer
integer = do
  digits <- T.unpack <$> P.takeWhileP (Just "digits") isDigit
  case readMaybe digits of
    Just x -> pure x
    Nothing -> fail $ "didn't parse an int" <> digits

--------------------------------------------------------------------------------
-- Helpers taken from the other parser
--------------------------------------------------------------------------------

-- edited a bit from the other parser
between :: Char -> Parser p -> Char -> Parser p
between fst p end = J.skipLiner fst *> J.spaceLiner p <* J.skipLiner end

parens :: Parser p -> Parser p
parens p = between Token.openParen p Token.closeParen
