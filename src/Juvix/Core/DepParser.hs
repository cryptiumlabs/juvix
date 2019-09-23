module Juvix.Core.DepParser where

import           Data.Functor.Identity
import           Juvix.Core.DepMainLang
import           Prelude
import           Text.Parsec
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token    as Token
    --Takes a string and output an ITerm.
    --iTerms :: Parser ITerms

languageDef ∷ GenLanguageDef String u Data.Functor.Identity.Identity
languageDef =
  emptyDef
    { Token.commentStart = "/*"
    , Token.commentEnd = "*/"
    , Token.commentLine = "//"
    , Token.identStart = letter
    , Token.identLetter = alphaNum
    , Token.reservedNames =
        [ "*"
        , "Nat"
        , "Zero" --ITerms without inputs
        , "Ann"
        , "Pi"
        , "Succ"
        , "NatElim" --ITerms with CTerms as inputs
        , "Vec"
        , "Nil"
        , "Cons"
        , "VecElim"
        , "Eq"
        , "Refl"
        , "EqElim"
        , "\\" --Bound var
        , "Free"
        , "Local"
        , "Quote"
        , "Global" --Free var
        ]
    , Token.reservedOpNames =
        [ "Inf"
        , "Lam"
        , ":"
        , "App" --application
        ]
    }

lexer ∷ Token.GenTokenParser String u Data.Functor.Identity.Identity
lexer = Token.makeTokenParser languageDef

identifier ∷ Parser String
identifier = Token.identifier lexer -- parses an identifier

reserved ∷ String → Parser ()
reserved = Token.reserved lexer -- parses a reserved name

reservedOp ∷ String → Parser ()
reservedOp = Token.reservedOp lexer -- parses an operator

parens ∷ Parser a → Parser a
parens = Token.parens lexer -- parses surrounding parenthesis:
                                        --   parens p
                                        -- takes care of the parenthesis and
                                        -- uses p to parse what's inside them

natural ∷ Parser Integer
natural = Token.natural lexer -- parses an integer

whiteSpace ∷ Parser ()
whiteSpace = Token.whiteSpace lexer -- parses whitespace

parseSimpleI ∷ (String, b) → Parser b
parseSimpleI (str, term) = reserved str >> return term
    --List of simple ITerms without inputs

reservedSimple ∷ [(String, ITerm)]
reservedSimple = [("*", Star), ("Nat", Nat), ("Zero", Zero)]

annTerm ∷ Parser ITerm
annTerm = do
  reserved "Ann"
  theTerm <- ctermOnly
  theType <- ctermOnly
  return $ Ann theTerm theType
   {-  <|> do
    theTerm <- ctermOnly --Idris annotation syntax
    reservedOp ":"
    theType <- ctermOnly
    return $ Ann theTerm theType
-}

piTerm ∷ Parser ITerm
piTerm = do
  reserved "Pi"
  input <- ctermOnly
  func <- ctermOnly
  return $ Pi input func

succTerm ∷ Parser ITerm
succTerm = do
  reserved "Succ"
  num <- ctermOnly
  return $ Succ num

natelimTerm ∷ Parser ITerm
natelimTerm = do
  reserved "NatElim"
  motive <- ctermOnly
  mZero <- ctermOnly
  inductive <- ctermOnly
  k <- ctermOnly
  return $ NatElim motive mZero inductive k

vecTerm ∷ Parser ITerm
vecTerm = do
  reserved "Vec"
  theType <- ctermOnly
  numOfElement <- ctermOnly
  return $ Vec theType numOfElement

nilTerm ∷ Parser ITerm
nilTerm = do
  reserved "Nil"
  theType <- ctermOnly
  return $ Nil theType

consTerm ∷ Parser ITerm
consTerm = do
  reserved "Cons"
  eleType <- ctermOnly
  numOfElement <- ctermOnly
  newEle <- ctermOnly
  theVec <- ctermOnly
  return $ Cons eleType numOfElement newEle theVec

vecelimTerm ∷ Parser ITerm
vecelimTerm = do
  reserved "VecElim"
  eleType <- ctermOnly
  motive <- ctermOnly
  mZero <- ctermOnly
  inductive <- ctermOnly
  numOfElement <- ctermOnly
  xs <- ctermOnly
  return $ VecElim eleType motive mZero inductive numOfElement xs

eqTerm ∷ Parser ITerm
eqTerm = do
  reserved "Eq"
  theType <- ctermOnly
  x <- ctermOnly
  y <- ctermOnly
  return $ Eq theType x y

reflTerm ∷ Parser ITerm
reflTerm = do
  reserved "Refl"
  theType <- ctermOnly
  x <- ctermOnly
  return $ Refl theType x

eqelimTerm ∷ Parser ITerm
eqelimTerm = do
  reserved "EqElim"
  theType <- ctermOnly
  motive <- ctermOnly
  refl <- ctermOnly
  x <- ctermOnly
  y <- ctermOnly
  eqxy <- ctermOnly
  return $ EqElim theType motive refl x y eqxy

boundTerm ∷ Parser ITerm
boundTerm = do
  reserved "Bound"
  index <- bIndex
  return $ Bound (fromInteger index)
    --Parser for de Bruijn indices of bound variables

bIndex ∷ Parser Integer
bIndex = parens bIndex <|> natural
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

name ∷ Parser Name
name = localTerm <|> quoteTerm <|> globalTerm

freeTerm ∷ Parser ITerm
freeTerm = do
  reserved "Free"
  fname <- name
  return $ Free fname

appTerm ∷ Parser ITerm
appTerm = do
  reservedOp "App"
  var <- iterm
  func <- ctermOnly
  eof
  return $ var :@: func

parseWhole ∷ Parser a → Parser a
parseWhole a = do
  whiteSpace
  aterm <- a
  whiteSpace
  eof
  return aterm

iterm ∷ Parser ITerm
iterm =
  parens iterm <|>
  foldr1 (<|>) (map parseSimpleI reservedSimple) --ITerms without inputs
   <|>
  annTerm --ITerms with CTerms as input(s).
   <|>
  piTerm <|>
  succTerm <|>
  natelimTerm <|>
  vecTerm <|>
  nilTerm <|>
  consTerm <|>
  vecelimTerm <|>
  eqTerm <|>
  reflTerm <|>
  eqelimTerm <|>
  boundTerm --Bound var
   <|>
  freeTerm --Free var
   <|>
  appTerm

infTerm ∷ Parser CTerm
infTerm = do
  reservedOp "Inf"
  iterm <- iterm
  return $ Inf iterm

lamTerm ∷ Parser CTerm
lamTerm = do
  reservedOp "\\"
  cTerm <- ctermOnly
  return $ Lam cTerm

convITerm ∷ Parser CTerm
convITerm = do
  theTerm <- iterm
  return $ Inf theTerm

cterm ∷ Parser CTerm
cterm = parens cterm <|> infTerm <|> lamTerm <|> convITerm

cOriTerm ∷ Parser (Either ITerm CTerm)
cOriTerm = Text.Parsec.try (Left <$> iterm) <|> (Right <$> cterm)

ctermOnly ∷ Parser CTerm
ctermOnly = do
  anyTerm <- cOriTerm
  return
    (case anyTerm of
       Left i  -> Inf i
       Right c -> c)

parseString ∷ Parser a → String → a
parseString p str =
  case parse p "" str of
    Left e  -> error $ show e
    Right r -> r
