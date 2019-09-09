{-# LANGUAGE ApplicativeDo #-}
module Juvix.Bohm.Parser where

import           Juvix.Library hiding ((<|>), many)
import           Prelude (String)

import           Text.Parsec
import           Text.ParserCombinators.Parsec.Language
import           Text.Parsec.String
import qualified Text.ParserCombinators.Parsec.Token as T
import           Text.Parsec.Expr                    as E
import           Control.Monad.Fail (fail)

import            Juvix.Bohm.Type

-- Lexer------------------------------------------------------------------------
langaugeDef :: Stream s m Char => GenLanguageDef s u m
langaugeDef = LanguageDef
              { T.reservedNames   = ["if", "then", "else", "let"
                                    , "letrec", "cons", "head", "tail", "nil"
                                    , "lambda", "in" , "true", "false", "isnil"]
              , T.reservedOpNames = [",", ":", ";", "(", ")", "[", "]", "{", "}"
                                    , ".", "+", "-", "*", "/", "=", "=="
                                    , "<>", "<", "<=", ">", ">=", "&", "|", ":="]
              , T.identStart      = letter   <|> char '_' <|> char '_'
              , T.identLetter     = alphaNum <|> char '_' <|> char '-'
              , T.caseSensitive   = True
              , commentStart      = "/*"
              , commentEnd        = "*/"
              , nestedComments    = True
              , identStart        = letter <|> char '_'
              , identLetter       = alphaNum <|> oneOf "_'"
              , opStart           = opLetter langaugeDef
              , opLetter          = oneOf ":!#$%&*+./<=>?@\\^|-~"
              , commentLine       = ""
              }

lexer :: Stream s m Char => T.GenTokenParser s u m
lexer = T.makeTokenParser langaugeDef


identifier :: Stream s m Char ⇒ ParsecT s u m String
natural    :: Stream s m Char ⇒ ParsecT s u m Integer
reserved   :: Stream s m Char ⇒ String → ParsecT s u m ()
reservedOp :: Stream s m Char ⇒ String → ParsecT s u m ()
semi       :: Stream s m Char ⇒ ParsecT s u m String
integer    :: Stream s m Char ⇒ ParsecT s u m Integer
whiteSpace :: Stream s m Char ⇒ ParsecT s u m ()
comma      :: Stream s m Char ⇒ ParsecT s u m String
brackets   :: Stream s m Char ⇒ ParsecT s u m a → ParsecT s u m a
parens     :: Stream s m Char ⇒ ParsecT s u m a → ParsecT s u m a
semiSep    :: Stream s m Char ⇒ ParsecT s u m a → ParsecT s u m [a]
braces     :: Stream s m Char ⇒ ParsecT s u m a → ParsecT s u m a
operator'  :: Stream s m Char ⇒ ParsecT s u m String

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
operator'  = T.operator   lexer

operator :: Stream s m Char ⇒ ParsecT s u m SomeSymbol
operator = someSymbolVal <$> operator'

symbol :: Stream s m Char ⇒ ParsecT s u m SomeSymbol
symbol = someSymbolVal <$> identifier

-- Grammar ---------------------------------------------------------------------

parseBohm :: String → Either ParseError Bohm
parseBohm = parseBohm' ""

parseBohm' :: SourceName → String → Either ParseError Bohm
parseBohm' = runParser (whiteSpace *> expression' <* eof) ()

parseBohmFile :: FilePath → IO (Either ParseError Bohm)
parseBohmFile fname = do
  input ← readFile fname
  pure $ parseBohm' fname (show input)

expression' :: Parser Bohm
expression' = ifThenElse
          <|> (application <?> "help")
          <|> cons
          <|> car
          <|> cdr
          <|> isNil
          <|> lambda
          <|> letExp
          <|> letRecExp
          <|> notExp
          <|> listExpression
          <|> trueLit
          <|> falseLit
          <|> intLit
          <|> symbol'

expression :: Parser Bohm
expression = buildExpressionParser optable expression'

listExpression :: Parser Bohm
listExpression = nil <|> listCase

-- Expression Parsers-----------------------------------------------------------
ifThenElse :: Parser Bohm
ifThenElse = do
  reserved "if"
  pred ← expression
  reserved "then"
  then' ← expression
  reserved "else"
  else' ← expression
  pure $ If pred then' else'

cons :: Parser Bohm
cons = do
  reserved "cons"
  (arg1,arg2) ← parens ((,) <$> expression <*> (reservedOp "," *> expression))
  pure $ Cons arg1 arg2

car :: Parser Bohm
car = do
  reserved "head"
  arg1 ← parens expression
  pure $ Car arg1

cdr :: Parser Bohm
cdr = do
  reserved "tail"
  arg1 ← parens expression
  pure $ Cdr arg1

isNil :: Parser Bohm
isNil = do
  reserved "isnil"
  arg1 ← parens expression
  pure $ IsNil arg1

intLit :: Parser Bohm
intLit = do
  int ← integer
  pure $ IntLit (fromInteger int)


lambda :: Parser Bohm
lambda = do
  reserved "lambda"
  sym ← symbol
  reservedOp "."
  exp ← expression
  pure $ Lambda sym exp

letExp :: Parser Bohm
letExp = do
  reserved "let"
  toBind ← symbol
  reservedOp "="
  binding ← expression
  reserved "in"
  body ← expression
  pure $ Let toBind binding body

letRecExp :: Parser Bohm
letRecExp = do
  reserved "letrec"
  toBind ← symbol
  reservedOp "="
  exp ← expression
  pure $ Letrec toBind exp

trueLit :: Parser Bohm
trueLit = True' <$ reserved "true"

falseLit :: Parser Bohm
falseLit = False' <$ reserved "true"

notExp :: Parser Bohm
notExp = do
  reserved "not"
  exp ← expression
  pure $ Not exp

application :: Parser Bohm
application = do
  app ← parens (many expression)
  case app of
    []     → fail "empty list"
    (x:xs) → pure $ foldl' Application x xs

symbol' :: Parser Bohm
symbol' = Symbol' <$> symbol

-- List Parser------------------------------------------------------------------

nil :: Parser Bohm
nil = Nil <$ reserved "nil"

listCase :: Parser Bohm
listCase = do
  exprs ← brackets (expression `sepBy` comma)
  pure $ foldr Cons Nil exprs

-- Infix------------------------------------------------------------------------

createOpTable :: ParsecT s u m (a → a → a) → Operator s u m a
createOpTable term = E.Infix term AssocLeft

createInfixOp :: String → Op → Parser (Bohm → Bohm → Bohm)
createInfixOp opStr term = Infix' term <$ reservedOp opStr

createInfix :: String → Op → Parser (Bohm → Bohm → Bohm)
createInfix opStr term = Infix' term <$ reserved opStr

listToChoiceOp :: [(String, Op)] → Parser (Bohm → Bohm → Bohm)
listToChoiceOp = choice . fmap (uncurry createInfixOp)

listToChoice :: [(String, Op)] → Parser (Bohm → Bohm → Bohm)
listToChoice = choice . fmap (uncurry createInfix)

infix0, infix1, infix2, infix3, infix4Op, infix4 :: Parser (Bohm → Bohm → Bohm)
infix0 = listToChoice [("or", Or), ("mod", Mod)]

infix1 = listToChoice [("and", And)]

infix2 = listToChoiceOp [("==", Eq), ("<>", Neq), ("<", Lt), ("<=", Le), (">", Gt), (">=", Ge)]

infix3 = listToChoiceOp [("+", Plus), ("-", Sub)]

infix4Op = listToChoiceOp [("*", Mult)]
infix4   = listToChoice   [("div", Division)]

optable :: [[Operator String () Identity Bohm]]
optable = [ [createOpTable infix4, createOpTable infix4Op]
          , [createOpTable infix3]
          , [createOpTable infix2]
          , [createOpTable infix1]
          , [createOpTable infix0]
          , [createOpTable (Infix' . Unkown <$> operator)]
          ]
