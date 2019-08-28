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
        , "App"
        ]
    , Token.reservedOpNames = ["Conv", "\\", ":"]
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
natw =   do reserved "w"
            return Omega
     <|> do n <- natural
            return $ Natural (fromInteger n)

sortTerm ∷ Parser CTerm
sortTerm =
  do reserved "*"
     n <- natural
     return $ Star (fromInteger n)

piTerm ∷ Parser CTerm
piTerm =
  do reserved "[Π]"
     pi <- natw
     input <- cterm
     func <- cterm
     return $ Pi pi input func

pmTerm ∷ Parser CTerm
pmTerm =
  do reserved "[π]"
     pm <- natw
     input <- cterm
     func <- cterm
     return $ Pm pm input func

paTerm ∷ Parser CTerm
paTerm =
  do reserved "/\\"
     pa <- natw
     input <- cterm
     func <- cterm
     return $ Pa pa input func

npmTerm ∷ Parser CTerm
npmTerm =
  do reserved "\\/"
     fst <- cterm
     snd <- cterm
     return $ NPm fst snd

lamTerm ∷ Parser CTerm
lamTerm =
  do reservedOp "\\"
     pi <- natw
     func <- cterm
     return $ Lam pi func

convTerm ∷ Parser CTerm
convTerm =
  do reservedOp "Conv"
     termToConvert <- iterm
     return $ Conv termToConvert

boundTerm ∷ Parser ITerm
boundTerm =
  do reserved "Bound"
     index <- natural
     return $ Bound (fromInteger index)

--Parser for the global free variable name
gName ∷ Parser String
gName =  parens gName
     <|> identifier

--Parser for Name data type
name ∷ Parser Name
name = do reserved "Local"
          index <- natural
          return $ Local (fromInteger index)
   <|> do reserved "Quote"
          index <- natural
          return $ Quote (fromInteger index)
   <|> do reserved "Global"
          gname <- gName
          return $ Global gname

freeTerm ∷ Parser ITerm
freeTerm =
  do reserved "Free"
     fname <- name
     return $ Free fname

appTerm ∷ Parser ITerm
appTerm =
  do reserved "App"
     pi <- natw
     func <- iterm
     var <- cterm
     eof
     return $ App pi func var

annTerm ∷ Parser ITerm
annTerm =
  do pi <- natw
     term <- cterm
     reservedOp ":"
     ann <- cterm
     eof
     return $ Ann pi term ann

parseWhole ∷ Parser a → Parser a
parseWhole p =
  do whiteSpace
     t <- p
     whiteSpace
     eof
     return t

cterm ∷ Parser CTerm
cterm = parens cterm
     <|> sortTerm
     <|> piTerm
     <|> pmTerm
     <|> paTerm
     <|> npmTerm
     <|> lamTerm
     <|> convTerm

iterm ∷ Parser ITerm
iterm =  parens iterm
     <|> boundTerm --Bound var
     <|> freeTerm --Free var<|> appTerm
     <|> appTerm
     <|> annTerm

parseString ∷ Parser a → String → Maybe a
parseString p str =
     case parse p "" str of
     Left _  -> Nothing
     Right r -> Just r
