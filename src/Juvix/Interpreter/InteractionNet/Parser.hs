{-# LANGUAGE ApplicativeDo #-}

module Juvix.Interpreter.InteractionNet.Parser where

import Control.Monad.Fail (fail)
import Juvix.Interpreter.InteractionNet.Default
import Juvix.Interpreter.InteractionNet.Shared hiding (symbol)
import Juvix.Interpreter.InteractionNet.Type
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
type Ops m = [[Operator String () m AST]]

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
    T.identStart =
      letter <|> char '_' <|> char '_',
    T.identLetter =
      alphaNum <|> char '_' <|> char '-',
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
identifier = T.identifier lexer

reserved ∷ Stream s m Char ⇒ String → ParsecT s u m ()
reserved = T.reserved lexer

reservedOp ∷ Stream s m Char ⇒ String → ParsecT s u m ()
reservedOp = T.reservedOp lexer

parens ∷ Stream s m Char ⇒ ParsecT s u m a → ParsecT s u m a
parens = T.parens lexer

integer ∷ Stream s m Char ⇒ ParsecT s u m Integer
integer = T.integer lexer

semi ∷ Stream s m Char ⇒ ParsecT s u m String
semi = T.semi lexer

semiSep ∷ Stream s m Char ⇒ ParsecT s u m a → ParsecT s u m [a]
semiSep = T.semiSep lexer

whiteSpace ∷ Stream s m Char ⇒ ParsecT s u m ()
whiteSpace = T.whiteSpace lexer

comma ∷ Stream s m Char ⇒ ParsecT s u m String
comma = T.comma lexer

braces ∷ Stream s m Char ⇒ ParsecT s u m a → ParsecT s u m a
braces = T.braces lexer

brackets ∷ Stream s m Char ⇒ ParsecT s u m a → ParsecT s u m a
brackets = T.brackets lexer

natural ∷ Stream s m Char ⇒ ParsecT s u m Integer
natural = T.natural lexer

operator' ∷ Stream s m Char ⇒ ParsecT s u m String
operator' = T.operator lexer

operator ∷ Stream s m Char ⇒ ParsecT s u m Symbol
operator = intern <$> operator'

symbol ∷ Stream s m Char ⇒ ParsecT s u m Symbol
symbol = intern <$> identifier

-- Grammar ---------------------------------------------------------------------

parseAST ∷ String → Either ParseError AST
parseAST = parseAST' ""

parseAST' ∷ SourceName → String → Either ParseError AST
parseAST' = runParser (whiteSpace *> expression' <* eof) ()

parseASTFile ∷ FilePath → IO (Either ParseError AST)
parseASTFile fname = do
  input ← readFile fname
  pure $ parseAST' fname (show input)

-- poor type signatures can't find the monadic version of parsec outside of stream
-- TODO ∷ rewrite this later

expression' ∷ ParsecT String () Identity AST
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

createInfixUnkown ∷ Symbol → AST → AST → AST
createInfixUnkown sym arg1 arg2 = Application (Application (Symbol' sym) arg1) arg2

-- So far only the defaultSpecial is sent in, but in the future, pass in extensions
-- to both defaultSpecial and defaultSymbols.
precedenceToOps ∷ Stream s m Char ⇒ OperatorTable s u m AST
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

expression ∷ Parser AST
expression = buildExpressionParser precedenceToOps expression'

listExpression ∷ ParsecT String () Identity AST
listExpression = nil <|> listCase

-- Expression Parser------------------------------------------------------------
ifThenElse ∷ ParsecT String () Identity AST
ifThenElse = do
  reserved "if"
  pred ← expression
  reserved "then"
  then' ← expression
  reserved "else"
  else' ← expression
  pure $ If pred then' else'

cons ∷ ParsecT String () Identity AST
cons = do
  reserved "cons"
  (arg1, arg2) ← parens ((,) <$> expression <*> (reservedOp "," *> expression))
  pure $ Cons arg1 arg2

car ∷ ParsecT String () Identity AST
car = do
  reserved "head"
  arg1 ← parens expression
  pure $ Car arg1

cdr ∷ ParsecT String () Identity AST
cdr = do
  reserved "tail"
  arg1 ← parens expression
  pure $ Cdr arg1

isNil ∷ ParsecT String () Identity AST
isNil = do
  reserved "isnil"
  arg1 ← parens expression
  pure $ IsNil arg1

intLit ∷ ParsecT String () Identity AST
intLit = do
  int ← integer
  pure $ IntLit (fromInteger int)

lambda ∷ ParsecT String () Identity AST
lambda = do
  reserved "lambda"
  sym ← symbol
  reservedOp "."
  exp ← expression
  pure $ Lambda sym exp

letExp ∷ ParsecT String () Identity AST
letExp = do
  reserved "let"
  toBind ← symbol
  reservedOp "="
  binding ← expression
  reserved "in"
  body ← expression
  pure $ Let toBind binding body

letRecExp ∷ ParsecT String () Identity AST
letRecExp = do
  reserved "letrec"
  toBind ← symbol
  reservedOp "="
  exp ← expression
  pure $ Letrec toBind exp

trueLit ∷ ParsecT String () Identity AST
trueLit = True' <$ reserved "true"

falseLit ∷ ParsecT String () Identity AST
falseLit = False' <$ reserved "true"

notExp ∷ ParsecT String () Identity AST
notExp = do
  reserved "not"
  exp ← expression
  pure $ Not exp

application ∷ ParsecT String () Identity AST
application = do
  app ← parens (many expression)
  case app of
    [] → fail "empty list"
    (x : xs) → pure $ foldl' Application x xs

symbol' ∷ ParsecT String () Identity AST
symbol' = Symbol' <$> symbol

-- List Parser------------------------------------------------------------------

nil ∷ ParsecT String () Identity AST
nil = Nil <$ reserved "nil"

listCase ∷ ParsecT String () Identity AST
listCase = do
  exprs ← brackets (expression `sepBy` comma)
  pure $ foldr Cons Nil exprs
