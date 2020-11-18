module Juvix.Core.HR.Parser where

import Data.Functor.Identity
import Juvix.Core.HR.Types
import Juvix.Core.Types
import Juvix.Library hiding ((<|>), try)
import Juvix.Library.Usage
import qualified Juvix.Library.NameSymbol as NameSymbol
import Text.Parsec hiding (try)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language hiding
  ( reservedNames,
    reservedOpNames,
  )
import qualified Text.ParserCombinators.Parsec.Token as Token
import Prelude (String)

baseReservedNames :: [String]
baseReservedNames =
  [ "*", -- sort
    "[Π]", -- function type
    "[Σ]", -- pair type
    "w" -- omega
  ]

baseReservedOpNames :: [String]
baseReservedOpNames =
  [ "\\", -- lambda
    "@", -- TODO: remove me, necessary for annotation parsing at the moment
    ":", -- type & usage annotation
    "->", -- arrow
    "|" -- before the universe level in annotations
  ]

generateParser ::
  forall primTy primVal.
  Parameterisation primTy primVal ->
  (String -> Maybe (Term primTy primVal))
generateParser parameterisation =
  let opNames :: [String]
      opNames = baseReservedOpNames <> reservedOpNames parameterisation
      --
      languageDef :: GenLanguageDef String u Identity
      languageDef =
        emptyDef
          { Token.commentStart = "/*",
            Token.commentEnd = "*/",
            Token.commentLine = "//",
            Token.identStart = letter,
            Token.identLetter = alphaNum,
            Token.reservedNames =
              baseReservedNames <> reservedNames parameterisation,
            Token.reservedOpNames = opNames
          }
      --
      ops :: [[Operator Char () (Elim primTy primVal)]]
      ops = [[Infix appl AssocLeft]]
      --
      appl ::
        Parser (Elim primTy primVal -> Elim primTy primVal -> Elim primTy primVal)
      appl = do
        whiteSpace
        notFollowedBy (choice (map reservedOp opNames))
        pure (\f x -> App f (Elim x))
      --
      lexer :: Token.GenTokenParser String u Identity
      lexer = Token.makeTokenParser languageDef
      --
      identifier :: Parser String
      identifier = Token.identifier lexer
      --
      reserved :: String -> Parser ()
      reserved = Token.reserved lexer
      --
      reservedOp :: String -> Parser ()
      reservedOp = Token.reservedOp lexer
      --
      parens :: Parser a -> Parser a
      parens = Token.parens lexer
      --
      natural :: Parser Integer
      natural = Token.natural lexer
      --
      whiteSpace :: Parser ()
      whiteSpace = Token.whiteSpace lexer
      --
      brackets :: Parser a -> Parser a
      brackets = Token.brackets lexer
      --
      comma :: Parser ()
      comma = void $ Token.comma lexer
      --
      usage :: Parser Usage
      usage = (reserved "w" >> return Omega) <|> SNat . fromInteger <$> natural
      --
      primTyTerm :: Parser (Term primTy primVal)
      primTyTerm = PrimTy |<< parseTy parameterisation lexer
      --
      primTerm :: Parser (Term primTy primVal)
      primTerm = Prim |<< parseVal parameterisation lexer
      --
      sortTerm :: Parser (Term primTy primVal)
      sortTerm =
        reserved "*"
          *> (Star . fromInteger <$> natural)
      --
      piTerm :: Parser (Term primTy primVal)
      piTerm =
        reserved "[Π]"
          *> (Pi <$> usage <*> binder <*> term <*> term)
      --
      lamTerm :: Parser (Term primTy primVal)
      lamTerm =
        Lam <$> (reservedOp "\\" *> binder)
          <*> (reservedOp "->" *> term)
      --
      sigTerm :: Parser (Term primTy primVal)
      sigTerm =
        reserved "[Σ]"
          *> (Sig <$> usage <*> binder <*> term <*> term)
      --
      pairTerm :: Parser (Term primTy primVal)
      pairTerm = brackets $ Pair <$> term <*> (comma *> term)
      --
      binder :: Parser NameSymbol.T
      binder = NameSymbol.fromString |<< identifier
      --
      term :: Parser (Term primTy primVal)
      term = try termOnly <|> elimTerm
      --
      termOnly :: Parser (Term primTy primVal)
      termOnly =
        parens termOnly <|> primTyTerm <|> try primTerm
          <|> sortTerm
          <|> piTerm
          <|> lamTerm
          <|> sigTerm
          <|> pairTerm
      --
      elimTerm :: Parser (Term primTy primVal)
      elimTerm = do
        elim <- elim
        pure (Elim elim)
      --
      annElim :: Parser (Elim primTy primVal)
      annElim = do
        reservedOp "@"
        theTerm <- term
        reservedOp ":"
        pi <- usage
        theType <- term
        reservedOp "|"
        level <- natural
        pure (Ann pi theTerm theType (fromIntegral level))
      --
      varElim :: Parser (Elim primTy primVal)
      varElim = Var |<< binder
      --
      elim :: Parser (Elim primTy primVal)
      elim = buildExpressionParser ops elim'
      --
      elim' :: Parser (Elim primTy primVal)
      elim' = annElim <|> varElim <|> parens elim
      --
      parseWhole :: Parser a -> Parser a
      parseWhole p = do
        whiteSpace
        t <- p
        whiteSpace
        eof
        return t
   in parseString' (parseWhole term)

parseString' :: Parser a -> String -> Maybe a
parseString' p str =
  case parse p "" str of
    Left _ -> Nothing
    Right r -> Just r
