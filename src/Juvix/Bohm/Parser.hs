{-# LANGUAGE ApplicativeDo #-}

module Juvix.Bohm.Parser where

import Control.Monad.Fail (fail)
import Juvix.Bohm.Default
import Juvix.Bohm.Shared hiding (symbol)
import Juvix.Bohm.Type
import Juvix.Library hiding ((<|>), many)
import qualified Juvix.Utility.HashMap as Map
import Juvix.Utility.Helper
import Text.Parsec
import Text.Parsec.Expr as E
import Text.Parsec.String
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as T
import Prelude (String)

-- Types------------------------------------------------------------------------

-- Ops ends up being recursive on itself
-- TODO ∷ Figure how to not make this dependent on itself
type Ops m = [[Operator String () m Bohm]]

-- Lexer------------------------------------------------------------------------
langaugeDef ∷ Stream s m Char ⇒ GenLanguageDef s u m
langaugeDef = LanguageDef
  { T.reservedNames =
      [ "if",
        "then",
        "else",
        "let",
        "letrec",
        "cons",
        "head",
        "tail",
        "nil",
        "lambda",
        "in",
        "true",
        "false",
        "isnil"
      ],
    T.reservedOpNames =
      [ ",",
        ":",
        ";",
        "(",
        ")",
        "[",
        "]",
        "{",
        "}",
        ".",
        "+",
        "-",
        "*",
        "/",
        "=",
        "==",
        "<>",
        "<",
        "<=",
        ">",
        ">=",
        "&",
        "|",
        ":="
      ],
    T.identStart = letter <|> char '_' <|> char '_',
    T.identLetter = alphaNum <|> char '_' <|> char '-',
    T.caseSensitive = True,
    commentStart = "/*",
    commentEnd = "*/",
    nestedComments = True,
    identStart = letter <|> char '_',
    identLetter = alphaNum <|> oneOf "_'",
    opStart = opLetter langaugeDef,
    opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~",
    commentLine = ""
  }

lexer ∷ Stream s m Char ⇒ T.GenTokenParser s u m
lexer = T.makeTokenParser langaugeDef

identifier ∷ Stream s m Char ⇒ ParsecT s u m String

natural ∷ Stream s m Char ⇒ ParsecT s u m Integer

reserved ∷ Stream s m Char ⇒ String → ParsecT s u m ()

reservedOp ∷ Stream s m Char ⇒ String → ParsecT s u m ()

semi ∷ Stream s m Char ⇒ ParsecT s u m String

integer ∷ Stream s m Char ⇒ ParsecT s u m Integer

whiteSpace ∷ Stream s m Char ⇒ ParsecT s u m ()

comma ∷ Stream s m Char ⇒ ParsecT s u m String

brackets ∷ Stream s m Char ⇒ ParsecT s u m a → ParsecT s u m a

parens ∷ Stream s m Char ⇒ ParsecT s u m a → ParsecT s u m a

semiSep ∷ Stream s m Char ⇒ ParsecT s u m a → ParsecT s u m [a]

braces ∷ Stream s m Char ⇒ ParsecT s u m a → ParsecT s u m a

operator' ∷ Stream s m Char ⇒ ParsecT s u m String

identifier = T.identifier lexer

reserved = T.reserved lexer

reservedOp = T.reservedOp lexer

parens = T.parens lexer

integer = T.integer lexer

semi = T.semi lexer

semiSep = T.semiSep lexer

whiteSpace = T.whiteSpace lexer

comma = T.comma lexer

braces = T.braces lexer

brackets = T.brackets lexer

natural = T.natural lexer

operator' = T.operator lexer

operator ∷ Stream s m Char ⇒ ParsecT s u m Symbol
operator = intern <$> operator'

symbol ∷ Stream s m Char ⇒ ParsecT s u m Symbol
symbol = intern <$> identifier

-- Grammar ---------------------------------------------------------------------

parseBohm ∷ String → Either ParseError Bohm
parseBohm = parseBohm' ""

parseBohm' ∷ SourceName → String → Either ParseError Bohm
parseBohm' = runParser (whiteSpace *> expression' <* eof) ()

parseBohmFile ∷ FilePath → IO (Either ParseError Bohm)
parseBohmFile fname = do
  input <- readFile fname
  pure $ parseBohm' fname (show input)

-- poor type signatures can't find the monadic version of parsec outside of stream
-- TODO ∷ rewrite this later

expression' ∷ ParsecT String () Identity Bohm
expression' =
  ifThenElse
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

-- Infix Parser ----------------------------------------------------------------

createInfixUnkown ∷ Symbol → Bohm → Bohm → Bohm
createInfixUnkown sym arg1 arg2 = Application (Application (Symbol' sym) arg1) arg2

-- So far only the defaultSpecial is sent in, but in the future, pass in extensions
-- to both defaultSpecial and defaultSymbols.
precedenceToOps ∷ Stream s m Char ⇒ OperatorTable s u m Bohm
precedenceToOps =
  ( \(Precedence _ s a) →
      let ins = intern s
       in E.Infix
            ( case defaultSpecial Map.!? ins of
                Just f → f <$ reservedOp s
                Nothing → createInfixUnkown ins <$ reservedOp s
            )
            a
  )
    <<$>> groupBy
      (\x y → level x == level y)
      (sortOnFlip level defaultSymbols)

expression ∷ Parser Bohm
expression = buildExpressionParser precedenceToOps expression'

listExpression ∷ ParsecT String () Identity Bohm
listExpression = nil <|> listCase

-- Expression Parser------------------------------------------------------------
ifThenElse ∷ ParsecT String () Identity Bohm
ifThenElse = do
  reserved "if"
  pred <- expression
  reserved "then"
  then' <- expression
  reserved "else"
  else' <- expression
  pure $ If pred then' else'

cons ∷ ParsecT String () Identity Bohm
cons = do
  reserved "cons"
  (arg1, arg2) <- parens ((,) <$> expression <*> (reservedOp "," *> expression))
  pure $ Cons arg1 arg2

car ∷ ParsecT String () Identity Bohm
car = do
  reserved "head"
  arg1 <- parens expression
  pure $ Car arg1

cdr ∷ ParsecT String () Identity Bohm
cdr = do
  reserved "tail"
  arg1 <- parens expression
  pure $ Cdr arg1

isNil ∷ ParsecT String () Identity Bohm
isNil = do
  reserved "isnil"
  arg1 <- parens expression
  pure $ IsNil arg1

intLit ∷ ParsecT String () Identity Bohm
intLit = do
  int <- integer
  pure $ IntLit (fromInteger int)

lambda ∷ ParsecT String () Identity Bohm
lambda = do
  reserved "lambda"
  sym <- symbol
  reservedOp "."
  exp <- expression
  pure $ Lambda sym exp

letExp ∷ ParsecT String () Identity Bohm
letExp = do
  reserved "let"
  toBind <- symbol
  reservedOp "="
  binding <- expression
  reserved "in"
  body <- expression
  pure $ Let toBind binding body

letRecExp ∷ ParsecT String () Identity Bohm
letRecExp = do
  reserved "letrec"
  toBind <- symbol
  reservedOp "="
  exp <- expression
  pure $ Letrec toBind exp

trueLit ∷ ParsecT String () Identity Bohm
trueLit = True' <$ reserved "true"

falseLit ∷ ParsecT String () Identity Bohm
falseLit = False' <$ reserved "true"

notExp ∷ ParsecT String () Identity Bohm
notExp = do
  reserved "not"
  exp <- expression
  pure $ Not exp

application ∷ ParsecT String () Identity Bohm
application = do
  app <- parens (many expression)
  case app of
    [] → fail "empty list"
    (x : xs) → pure $ foldl' Application x xs

symbol' ∷ ParsecT String () Identity Bohm
symbol' = Symbol' <$> symbol

-- List Parser------------------------------------------------------------------

nil ∷ ParsecT String () Identity Bohm
nil = Nil <$ reserved "nil"

listCase ∷ ParsecT String () Identity Bohm
listCase = do
  exprs <- brackets (expression `sepBy` comma)
  pure $ foldr Cons Nil exprs
