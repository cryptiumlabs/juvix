module Juvix.Core.Parser where

import           Data.Functor.Identity
import           Juvix.Core.MainLang
import           Prelude
import           Text.Parsec
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token    as Token

languageDef ∷ GenLanguageDef String u Data.Functor.Identity.Identity
languageDef =
  emptyDef
    { Token.commentStart = "/*"
    , Token.commentEnd = "*/"
    , Token.commentLine = "//"
    , Token.identStart = letter
    , Token.identLetter = alphaNum
    , Token.reservedNames =
        [ "*" --sort
        , "[Π]" --function type
        , "[π]" --dependent multiplicative conjunction type
        , "/\\" --dependent additive conjunction type
        , "\\/" --non-dependent multiplicative disjunction type
        , "Bound" --Bound var
        , "Free"
        , "Local"
        , "Quote"
        , "Global" --Free var
        , "w" --Omega
        , "App" --application
        ]
    , Token.reservedOpNames = ["Conv", "\\", ":", "cType"]
    }

lexer ∷ Token.GenTokenParser String u Data.Functor.Identity.Identity
lexer = Token.makeTokenParser languageDef
     -- These are parsers for what their names signify

identifier ∷ Parser String
identifier = Token.identifier lexer

reserved ∷ String → Parser ()
reserved = Token.reserved lexer

reservedOp ∷ String → Parser ()
reservedOp = Token.reservedOp lexer

parens ∷ Parser a → Parser a
parens = Token.parens lexer -- parses surrounding parenthesis, and what is inside them

natural ∷ Parser Integer
natural = Token.natural lexer

whiteSpace ∷ Parser ()
whiteSpace = Token.whiteSpace lexer

natw ∷ Parser NatAndw
natw =
  do reserved "w"
     return Omega
     <|> do
    n <- natural
    return $ SNat (fromInteger n)

sortTerm ∷ Parser CTerm
sortTerm = do
  reserved "*"
  n <- natural
  return $ Star (fromInteger n)

piTerm ∷ Parser CTerm
piTerm = do
  reserved "[Π]"
  pi <- natw
  input <- ctermOnly
  func <- ctermOnly
  return $ Pi pi input func

pmTerm ∷ Parser CTerm
pmTerm = do
  reserved "[π]"
  pm <- natw
  input <- ctermOnly
  func <- ctermOnly
  return $ Pm pm input func

paTerm ∷ Parser CTerm
paTerm = do
  reserved "/\\"
  pa <- natw
  input <- ctermOnly
  func <- ctermOnly
  return $ Pa pa input func

npmTerm ∷ Parser CTerm
npmTerm = do
  reserved "\\/"
  fst <- ctermOnly
  snd <- ctermOnly
  return $ NPm fst snd

lamTerm ∷ Parser CTerm
lamTerm = do
  reservedOp "\\"
  func <- ctermOnly
  return $ Lam func

convTerm ∷ Parser CTerm
convTerm = do
  reservedOp "Conv"
  termToConvert <- iterm
  return $ Conv termToConvert

boundTerm ∷ Parser ITerm
boundTerm = do
  reserved "Bound"
  index <- natural
  return $ Bound (fromInteger index)

--Parser for the global free variable name
gName ∷ Parser String
gName = parens gName <|> identifier

--Parser for Name data type
localTerm ∷ Parser Name
localTerm = do
  reserved "Local"
  index <- natural
  return $ Local (fromInteger index)

quoteTerm ∷ Parser Name
quoteTerm = do
  reserved "Quote"
  index <- natural
  return $ Quote (fromInteger index)

globalTerm ∷ Parser Name
globalTerm = do
  reserved "Global"
  gname <- gName
  return $ Global gname

pName ∷ Parser Name
pName = localTerm <|> quoteTerm <|> globalTerm

freeTerm ∷ Parser ITerm
freeTerm = do
  reserved "Free"
  fname <- pName
  return $ Free fname

appTerm ∷ Parser ITerm
appTerm = do
  reserved "App"
  func <- iterm
  var <- ctermOnly
  eof
  return $ App func var

annTerm ∷ Parser ITerm
annTerm = do
  pi <- natw
  term <- ctermOnly
  reservedOp ":"
  ann <- ctermOnly
  eof
  return $ Ann pi term ann

parseWhole ∷ Parser a → Parser a
parseWhole p = do
  whiteSpace
  t <- p
  whiteSpace
  eof
  return t

cterm ∷ Parser CTerm
cterm =
  parens cterm <|> sortTerm <|> piTerm <|> pmTerm <|> paTerm <|> npmTerm <|>
  lamTerm <|>
  convTerm <|>
  convITerm

convITerm ∷ Parser CTerm
convITerm = do
  theTerm <- iterm
  return $ Conv theTerm

iterm ∷ Parser ITerm
iterm = parens iterm <|> boundTerm <|> freeTerm <|> appTerm <|> annTerm

cOriTerm ∷ Parser (Either ITerm CTerm)
cOriTerm = Text.Parsec.try (Left <$> iterm) <|> (Right <$> cterm)

ctermOnly ∷ Parser CTerm
ctermOnly = do
  anyTerm <- cOriTerm
  return
    (case anyTerm of
       Left i  -> Conv i
       Right c -> c)

--the type checker takes in a term, its usage and type, and returns ...
pCType ∷ Parser (Result ())
pCType = do
  reservedOp "cType"
  theTerm <- ctermOnly
  usage <- natw
  theType <- ctermOnly
  return $ cType 0 [] theTerm (usage, cEval theType [])

parseString ∷ Parser a → String → Maybe a
parseString p str =
  case parse p "" str of
    Left _  -> Nothing
    Right r -> Just r
