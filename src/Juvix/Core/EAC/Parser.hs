{-# LANGUAGE ApplicativeDo #-}

module Juvix.Core.EAC.Parser
  ( parseEal,
    parseEal',
    parseEalFile,
    types,
  )
where

import Juvix.Core.EAC.Types hiding (PType, RPT, RPTI, RPTO)
import qualified Juvix.Core.EAC.Types as EAC
import qualified Juvix.Core.Parameterisations.Unit as Unit
import Juvix.Library hiding
  ( (<|>),
    Type,
    link,
    many,
    optional,
    reduce,
    try,
  )
import Text.Parsec
import Text.Parsec.Expr as E
import Text.Parsec.String
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as T
import Prelude (String, error)

type PType = EAC.PType Unit.Ty

type RPTO = EAC.RPTO Unit.Val

type RPTI = EAC.RPTI Unit.Val

langaugeDef ∷ Stream s m Char ⇒ GenLanguageDef s u m
langaugeDef = LanguageDef
  { T.reservedNames = ["lambda"],
    T.reservedOpNames =
      [ ",",
        ":",
        "(",
        ")",
        "[",
        "]",
        "{",
        "}",
        "λ",
        ".",
        "!",
        "!-",
        "-o"
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
    opLetter = oneOf ":#$%&*+./<=>?@\\^|-~",
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

whiteSpace ∷ Stream s m Char ⇒ ParsecT s u m ()
whiteSpace = T.whiteSpace lexer

-- Full Parsers ----------------------------------------------------------------
parseEal ∷ String → Either ParseError RPTO
parseEal = parseEal' ""

parseEal' ∷ SourceName → String → Either ParseError RPTO
parseEal' = runParser (whiteSpace *> expression <* eof) ()

parseEalFile ∷ FilePath → IO (Either ParseError RPTO)
parseEalFile fname = do
  input ← readFile fname
  pure $ parseEal' fname (show input)

-- Grammar ---------------------------------------------------------------------
expressionGen ∷ Stream s m Char ⇒ ParsecT s u m RPTI → ParsecT s u m RPTO
expressionGen ealGen = do
  bang ← try (string "!-") <|> string "!"
  bangs ← many (try (string "!-") <|> string "!" <|> string " ")
  term ← ealGen
  let f "!" = 1
      f _ = (- 1)
      num = sum (fmap f (filter (/= " ") (bang : bangs)))
  pure (RBang num term)

expression ∷ Parser RPTO
expression = expressionGen eal <|> (RBang 0 <$> eal)

expression' ∷ Parser RPTO
expression' = expressionGen eal' <|> (RBang 0 <$> eal')

eal ∷ Parser RPTI
eal =
  try (application <?> "Application")
    <|> (lambda <?> "RLam")
    <|> (term <?> "RPTO")
    <|> parens eal

eal' ∷ Parser RPTI
eal' =
  try (parens eal')
    <|> (lambda <?> "RLam")
    <|> (term <?> "RPTO")
    -- Eal here is safe when it's in a ()'s
    <|> parens eal

types ∷ Parser PType
types = buildExpressionParser optable types'

types' ∷ Parser PType
types' = bangs <|> specific

symbol ∷ Stream s m Char ⇒ ParsecT s u m Symbol
symbol = intern <$> identifier

lambda ∷ Parser RPTI
lambda = do
  reserved "lambda" <|> reservedOp "\\" <|> reservedOp "λ"
  s ← symbol
  reservedOp "."
  body ← expression
  pure (RLam s body)

application ∷ Parser RPTI
application = do
  exp ← expression'
  exps ← many1 expression'
  case exps of
    [] → error "doesn't happen"
    x : xs → pure $ foldl (\acc x → RApp (RBang 0 acc) x) (RApp exp x) xs

term ∷ Parser RPTI
term = RVar <$> symbol

bangs ∷ Parser PType
bangs = do
  bangs ← many1 (try (string "!-") <|> string "!" <|> string " ")
  typ ← parens types <|> types
  let f "!" = 1
      f _ = (- 1)
      num = sum (fmap f (filter (/= " ") bangs))
  pure $ addSymT num typ

addSymT ∷ Param → PType → PType
addSymT n (PSymT a b) = PSymT (n + a) b
addSymT n (PArrT a b c) = PArrT (n + a) b c
addSymT _ (PPrimT p) = PPrimT p

specific ∷ Parser PType
specific = PSymT 0 <$> symbol

createOpTable ∷ ParsecT s u m (a → a → a) → Operator s u m a
createOpTable term = E.Infix term AssocRight

lolly ∷ Stream s m Char ⇒ ParsecT s u m (PType → PType → PType)
lolly = PArrT 0 <$ reservedOp "-o"

optable ∷ [[Operator String () Identity PType]]
optable = [[createOpTable lolly]]
