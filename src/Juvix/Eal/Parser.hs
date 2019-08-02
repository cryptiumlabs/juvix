{-# LANGUAGE ApplicativeDo #-}
module Juvix.Eal.Parser where


import           Prelude                                (String)
import           Text.Parsec
import           Text.Parsec.Expr                       as E
import           Text.Parsec.String
import           Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token    as T

import           Juvix.Eal.Types
import           Juvix.Library                          hiding (link, many,
                                                         optional, reduce, try,
                                                         (<|>))

langaugeDef ∷ Stream s m Char ⇒ GenLanguageDef s u m
langaugeDef = LanguageDef
              { T.reservedNames   = ["lambda", "forall", "Forall"]
              , T.reservedOpNames = [",", ":", "(", ")", "[", "]", "{", "}"
                                    , "λ", ".", "!", "!-", "-o"]
              , T.identStart      = letter   <|> char '_' <|> char '_'
              , T.identLetter     = alphaNum <|> char '_' <|> char '-'
              , T.caseSensitive   = True
              , commentStart      = "/*"
              , commentEnd        = "*/"
              , nestedComments    = True
              , identStart        = letter <|> char '_'
              , identLetter       = alphaNum <|> oneOf "_'"
              , opStart           = opLetter langaugeDef
              , opLetter          = oneOf ":#$%&*+./<=>?@\\^|-~"
              , commentLine       = ""
              }

lexer ∷ Stream s m Char ⇒ T.GenTokenParser s u m
lexer = T.makeTokenParser langaugeDef

identifier ∷ Stream s m Char ⇒ ParsecT s u m String
natural    ∷ Stream s m Char ⇒ ParsecT s u m Integer
reserved   ∷ Stream s m Char ⇒ String → ParsecT s u m ()
reservedOp ∷ Stream s m Char ⇒ String → ParsecT s u m ()
semi       ∷ Stream s m Char ⇒ ParsecT s u m String
integer    ∷ Stream s m Char ⇒ ParsecT s u m Integer
whiteSpace ∷ Stream s m Char ⇒ ParsecT s u m ()
comma      ∷ Stream s m Char ⇒ ParsecT s u m String
brackets   ∷ Stream s m Char ⇒ ParsecT s u m a → ParsecT s u m a
parens     ∷ Stream s m Char ⇒ ParsecT s u m a → ParsecT s u m a
semiSep    ∷ Stream s m Char ⇒ ParsecT s u m a → ParsecT s u m [a]
braces     ∷ Stream s m Char ⇒ ParsecT s u m a → ParsecT s u m a

identifier = T.identifier lexer
reserved   = T.reserved   lexer
reservedOp = T.reservedOp lexer
parens     = T.parens     lexer
integer    = T.integer    lexer
semi       = T.semi       lexer
semiSep    = T.semiSep    lexer
whiteSpace = T.whiteSpace lexer
comma      = T.comma      lexer
braces     = T.braces     lexer
brackets   = T.brackets   lexer
natural    = T.natural    lexer

-- Full Parsers ----------------------------------------------------------------
parseEal ∷ String → Either ParseError Term
parseEal = parseEal' ""

parseEal' ∷ SourceName → String → Either ParseError Term
parseEal' = runParser (whiteSpace *> expression <* eof) ()

parseBohmFile ∷ FilePath → IO (Either ParseError Term)
parseBohmFile fname = do
  input ← readFile fname
  pure $ parseEal' fname (show input)

-- Grammar ---------------------------------------------------------------------
expression ∷ Parser Term
expression = do
  bangs ← many (try (string "!-") <|> string "!" <|> string " ")
  term ← try eal <|> parens eal
  let f "!" = 1
      f _   = (-1)
      num = sum (fmap f (filter (/= " ") bangs))
  pure (Bang num term)

eal ∷ Parser Eal
eal = (lambda <?> "Lambda")
   <|> (term <?> "Term")
   <|> (application <?> "Application")

types ∷ Parser Types
types = buildExpressionParser optable types'

types' ∷ Parser Types
types' = bangs <|> forall <|> specific

symbol ∷ Stream s m Char ⇒ ParsecT s u m SomeSymbol
symbol = someSymbolVal <$> identifier

lambda ∷ Parser Eal
lambda = do
  reserved "lambda" <|> reservedOp "\\" <|> reservedOp "λ"
  s ← symbol
  reservedOp ":"
  sType ← types
  reservedOp "."
  body ← expression
  pure (Lambda s sType body)

application ∷ Parser Eal
application =
  parens (App <$> expression <*> expression)

term ∷ Parser Eal
term = Term <$> symbol

--  isLolly ← optional (reservedOp "-o")

bangs ∷ Parser Types
bangs = do
  bangs ← many1 (try (string "!-") <|> string "!" <|> string " ")
  typ   ← parens types <|> types
  let f "!" = 1
      f _   = (-1)
      num = sum (fmap f (filter (/= " ") bangs))
  pure $
    if | num == 0  → typ
       | num >  0  → BangT num typ
       | otherwise → UBang num typ

forall ∷ Parser Types
forall = Forall <$ (reserved "forall" <|> reserved "Forall")

specific ∷ Parser Types
specific = Specific <$> symbol

createOpTable ∷ ParsecT s u m (a → a → a) → Operator s u m a
createOpTable term = E.Infix term AssocRight

lolly ∷ Stream s m Char ⇒ ParsecT s u m (Types → Types → Types)
lolly = Lolly <$ reservedOp "-o"

optable ∷ [[Operator String () Identity Types]]
optable = [[createOpTable lolly]]
