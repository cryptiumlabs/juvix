module Juvix.Core.HR.Parser where

import Data.Functor.Identity
import Juvix.Core.HR.Types
import Juvix.Core.Types
import Juvix.Core.Usage
import Juvix.Library hiding ((<|>), try)
import Text.Parsec hiding (try)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language hiding
  ( reservedNames,
    reservedOpNames,
  )
import qualified Text.ParserCombinators.Parsec.Token as Token
import Prelude (String)

baseReservedNames ∷ [String]
baseReservedNames =
  [ "*", -- sort
    "[Π]", -- function type
    "w" -- omega
  ]

baseReservedOpNames ∷ [String]
baseReservedOpNames =
  [ "\\", -- lambda
    "@", -- TODO: remove me, necessary for annotation parsing at the moment
    ":", -- type & usage annotation
    "->" -- arrow
  ]

generateParser ∷
  ∀ primTy primVal.
  Parameterisation primTy primVal →
  (String → Maybe (Term primTy primVal))
generateParser parameterisation =
  let opNames ∷ [String]
      opNames = baseReservedOpNames <> reservedOpNames parameterisation

      languageDef ∷ GenLanguageDef String u Identity
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

      ops ∷ [[Operator Char () (Elim primTy primVal)]]
      ops = [[Infix appl AssocLeft]]

      appl ∷
        Parser (Elim primTy primVal → Elim primTy primVal → Elim primTy primVal)
      appl = do
        whiteSpace
        notFollowedBy (choice (map reservedOp opNames))
        pure (\f x → App f (Elim x))

      lexer ∷ Token.GenTokenParser String u Identity
      lexer = Token.makeTokenParser languageDef

      identifier ∷ Parser String
      identifier = Token.identifier lexer

      reserved ∷ String → Parser ()
      reserved = Token.reserved lexer

      reservedOp ∷ String → Parser ()
      reservedOp = Token.reservedOp lexer

      parens ∷ Parser a → Parser a
      parens = Token.parens lexer

      natural ∷ Parser Integer
      natural = Token.natural lexer

      whiteSpace ∷ Parser ()
      whiteSpace = Token.whiteSpace lexer

      usage ∷ Parser Usage
      usage = (reserved "w" >> return Omega) <|> SNat . fromInteger <$> natural

      primTyTerm ∷ Parser (Term primTy primVal)
      primTyTerm = PrimTy |<< parseTy parameterisation lexer

      sortTerm ∷ Parser (Term primTy primVal)
      sortTerm = do
        reserved "*"
        n ← natural
        return $ Star (fromInteger n)

      piTerm ∷ Parser (Term primTy primVal)
      piTerm = do
        reserved "[Π]"
        pi ← usage
        input ← term
        func ← term
        return $ Pi pi input func

      lamTerm ∷ Parser (Term primTy primVal)
      lamTerm = do
        reservedOp "\\"
        binder ← binder
        reservedOp "->"
        func ← term
        return $ Lam binder func

      binder ∷ Parser Symbol
      binder = intern |<< identifier

      term ∷ Parser (Term primTy primVal)
      term = try termOnly <|> elimTerm

      termOnly ∷ Parser (Term primTy primVal)
      termOnly =
        parens termOnly <|> primTyTerm <|> sortTerm <|> piTerm <|> lamTerm

      elimTerm ∷ Parser (Term primTy primVal)
      elimTerm = do
        elim ← elim
        pure (Elim elim)

      primElim ∷ Parser (Elim primTy primVal)
      primElim = Prim |<< parseVal parameterisation lexer

      annElim ∷ Parser (Elim primTy primVal)
      annElim = do
        reservedOp "@"
        theTerm ← term
        reservedOp ":"
        pi ← usage
        theType ← term
        pure (Ann pi theTerm theType)

      varElim ∷ Parser (Elim primTy primVal)
      varElim = Var |<< binder

      elim ∷ Parser (Elim primTy primVal)
      elim = buildExpressionParser ops elim'

      elim' ∷ Parser (Elim primTy primVal)
      elim' = try primElim <|> annElim <|> varElim <|> parens elim

      parseWhole ∷ Parser a → Parser a
      parseWhole p = do
        whiteSpace
        t ← p
        whiteSpace
        eof
        return t

   in parseString' (parseWhole term)

parseString' ∷ Parser a → String → Maybe a
parseString' p str =
  case parse p "" str of
    Left _ → Nothing
    Right r → Just r
