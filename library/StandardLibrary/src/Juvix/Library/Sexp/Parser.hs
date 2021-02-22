module Juvix.Library.Sexp.Parser (parse) where

import qualified Data.Attoparsec.ByteString as Atto
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Text.Encoding as Encoding
import Juvix.Library hiding (list)
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.Sexp.Types as Sexp
import qualified Juvix.Library.Symbol.Lexer as Lexer
import Prelude (String, fail)

-- | @parse@ parses any sexp expression into the Sexp type
parse :: ByteString -> Either String Sexp.T
parse = Atto.parseOnly (eatSpaces sexp)

--------------------------------------------------------------------------------
-- Sexp Main Parsers
--------------------------------------------------------------------------------
sexp :: Atto.Parser Sexp.T
sexp = spaceLiner (list <|> (Sexp.Atom <$> atom))

list :: Atto.Parser Sexp.T
list = do
  d <- parens (many sexp)
  case d of
    [] -> pure Sexp.Nil
    _ -> pure (foldr Sexp.Cons Sexp.Nil d)

atom :: Atto.Parser Sexp.Atom
atom = number <|> name

name :: Atto.Parser Sexp.Atom
name = do
  sym <- symbol
  pure (Sexp.A sym Nothing)

number :: Atto.Parser Sexp.Atom
number = do
  int <- integer
  pure (Sexp.N int Nothing)

symbol :: Atto.Parser NameSymbol.T
symbol = do
  s <-
    Atto.takeWhile1
      ( \x ->
          Lexer.validStartSymbol x
            || Lexer.validMiddleSymbol x
            || Lexer.validInfixSymbol x
      )
  Encoding.decodeUtf8 s |> internText |> NameSymbol.fromSymbol |> pure

-- Code stolen from the other parser ☹

integer :: Atto.Parser Integer
integer = do
  digits <- Atto.takeWhile Lexer.digit
  case Char8.readInteger digits of
    Just (x, _) -> pure x
    Nothing -> fail "didn't parse an int"

--------------------------------------------------------------------------------
-- Helpers taken from the other parser
--------------------------------------------------------------------------------

-- edited a bit from the other parser
between :: Word8 -> Atto.Parser p -> Word8 -> Atto.Parser p
between fst p end = skipLiner fst *> spaceLiner p <* skipLiner end

parens :: Atto.Parser p -> Atto.Parser p
parens p = between Lexer.openParen p Lexer.closeParen

eatSpaces :: Atto.Parser p -> Atto.Parser p
eatSpaces p = Atto.takeWhile emptyCheck *> p

skipLiner :: Word8 -> Atto.Parser ()
skipLiner p = spaceLiner (Atto.skip (== p))

spaceLiner :: Atto.Parser p -> Atto.Parser p
spaceLiner p = p <* Atto.takeWhile emptyCheck

emptyCheck :: Word8 -> Bool
emptyCheck x = Lexer.space == x || Lexer.endOfLine x
