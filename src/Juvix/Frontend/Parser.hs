{-# LANGUAGE ApplicativeDo #-}

-- |
-- - The front end parser for the Juvix Programming language
--
-- - Parsers with S at the end, eat the spaces at the end of the parse
--
-- - Parsers with SN at the end, eats the spaces and new lines at the
--   end of the parse
module Juvix.Frontend.Parser where

import Data.Attoparsec.ByteString
import qualified Data.ByteString as ByteString
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
import qualified Data.Text.Encoding as Encoding
import qualified Juvix.Core.Usage as Usage
import qualified Juvix.Frontend.Lexer as Lexer
import qualified Juvix.Frontend.Types as Types
import Juvix.Library hiding (maybe, option, product, sum, takeWhile, try)
import Prelude (fail)

--------------------------------------------------------------------------------
-- Top Level
--------------------------------------------------------------------------------

topLevel = typeP <|> function

expression = fail "foo"

usage = string "u#" *> expression

function = undefined

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

typeP ∷ Parser Types.Type
typeP = do
  _ ← spaceLiner (string "type")
  usag ← maybe usage
  name ← prefixSymbolSN
  args ← many prefixSymbolSN
  form ← typeSumParser
  pure (Types.Typ usag name args form)

typeSumParser ∷ Parser Types.TypeSum
typeSumParser =
  Types.NewType <$> try newTypeParser
    <|> Types.Alias <$> try aliasParser
    <|> Types.Data <$> dataParser

newTypeParser ∷ Parser Types.NewType
newTypeParser = do
  let nonOverlappingCase = do
        p ← peekWord8
        case p of
          Just p →
            if
              | p == Lexer.dash || p == Lexer.dash || p == Lexer.colon →
                fail "overlapping"
              | otherwise → pure ()
          Nothing → pure ()
  spaceLiner (skip (== Lexer.equals))
  spaceLiner (skip (== Lexer.pipe))
  -- if we get a | or a - at the end of this, then we need to go to the other case
  -- Note that we may end up with a non boxed type, but that is fine
  -- this is a subset of the ADT case for analysis
  Types.Declare <$> prefixSymbolSN <*> typeRefineSN
    <* nonOverlappingCase

aliasParser ∷ Parser Types.Alias
aliasParser = do
  spaceLiner (skip (== Lexer.equals))
  Types.AliasDec <$> typeRefine

dataParser ∷ Parser Types.Data
dataParser = do
  arrow ← maybe (spaceLiner (skip (== Lexer.colon)) *> arrowTypeSN)
  spaceLiner (skip (== Lexer.equals))
  adt ← adt
  case arrow of
    Just arr → pure (Types.Arrowed arr adt)
    Nothing → pure (Types.NonArrowed adt)

--------------------------------------------------
-- ADT parser
--------------------------------------------------

adt ∷ Parser Types.Adt
adt =
  Types.Sum <$> many1H sumSN
    <|> Types.Product <$> product

sum ∷ Parser Types.Sum
sum = do
  spaceLiner (skip (== Lexer.pipe))
  Types.S <$> prefixSymbolSN <*> maybe product

product ∷ Parser Types.Product
product =
  Types.Record <$> record
    <|> Types.Arrow <$> arrowType

record ∷ Parser Types.Record
record = do
  names ←
    spaceLiner
      $ curly
      $ do
        commas ←
          many $
            spaceLiner nameType <* spaceLiner (skip (== Lexer.comma))
        last ← option [] (pure <$> spaceLiner nameType)
        pure (commas <> last)
  case names of
    _ : _ → do
      --
      let names' = NonEmpty.fromList names
      --
      familySignature ← maybe (spaceLiner (string ":") *> typeRefine)
      pure (Types.Record' names' familySignature)
    [] → fail "must have at least one item in a record"

nameType ∷ Parser Types.NameType
nameType = do
  name ← nameParserSN
  spaceLiner (skip (== Lexer.colon))
  sig ← arrowType
  pure (Types.NameType sig name)

nameParserColon ∷ Parser Types.Name
nameParserColon =
  nameParserSN <* skip (== Lexer.colon)

nameParser ∷ Parser Types.Name
nameParser =
  (skip (== Lexer.hash) *> fmap Types.Implicit prefixSymbol)
    <|> Types.Concrete <$> prefixSymbol

--------------------------------------------------
-- Arrow Type parser
--------------------------------------------------

-- This family of parsers are a bit complicated
-- Though they work well!

arrowType ∷ Parser Types.ArrowType
arrowType = do
  let toType (Arrow a) = Types.Arrows a
      toType (Parens p) = Types.Parens p
  recursive ← arrowOrParens
  end ← fmap Types.Refined (namedRefine <|> parens namedRefine) <|> endArrow
  pure (foldr toType end recursive)

data ArrowOrParens
  = Arrow Types.ArrowData
  | Parens Types.ArrowParen
  deriving (Show)

endArrow ∷ Parser Types.ArrowType
endArrow = Types.End <$> parens arrowType

arrowOrParens ∷ Parser [ArrowOrParens]
arrowOrParens =
  many (Arrow <$> arrowsSN <|> Parens <$> parendArrowSN)

arrowGen ∷ Parser a → Parser (Types.ArrowGen a)
arrowGen p =
  Types.ArrGen <$> maybe nameParserColonSN <*> p <*> arrowSymbol

arrows ∷ Parser Types.ArrowData
arrows =
  Types.Arr <$> arrowGen typeRefineSN

parendArrow ∷ Parser Types.ArrowParen
parendArrow =
  Types.Paren <$> parens (arrowGen arrowTypeSN)

namedRefine ∷ Parser Types.NamedRefine
namedRefine =
  Types.NamedRefine <$> maybe nameParserColonSN <*> typeRefine

arrowSymbol ∷ Parser Types.ArrowSymbol
arrowSymbol =
  Types.ArrowUse Usage.Omega <$ string "->"
    <|> Types.ArrowUse one <$ string "-o"
    <|> Types.ArrowUse mempty <$ string "-|"
    <|> ( do
            spaceLiner (skip (== Lexer.dash))
            exp ← expressionSN
            string "->"
            pure (Types.ArrowExp exp)
        )

--------------------------------------------------
-- TypeNameParser and typeRefine Parser
--------------------------------------------------

typeRefine ∷ Parser Types.TypeRefine
typeRefine = do
  typeName ← typeNameParserSN
  refine ← maybe (curly expressionSN)
  pure (Types.TypeRefine typeName refine)

typeNameParser ∷ Parser Types.TypeName
typeNameParser = do
  pre ← prefixSymbolSN
  body ←
    many
      $ spaceLiner
      $ universeSymbol
        <|> Types.SymbolName <$> prefixSymbolSN
        <|> Types.ArrowName <$> parens arrowTypeSN
  pure (Types.Start pre body)

universeSymbol ∷ Parser Types.TypeNameValid
universeSymbol = do
  _ ← string "u#"
  Types.UniverseName <$> universeExpression

universeExpression ∷ Parser Types.UniverseExpression
universeExpression =
  Types.UniverseExpression <$> prefixSymbolSN
    -- TODO ∷ make this proper do + and max!
    <|> Types.UniverseExpression <$> parens prefixSymbolSN

--------------------------------------------------------------------------------
-- Symbol Handlers
--------------------------------------------------------------------------------

prefixSymbol ∷ Parser Symbol
prefixSymbol = do
  start ← satisfy Lexer.validStartSymbol
  rest ← takeWhile Lexer.validMiddleSymbol
  -- Slow O(n) call, could maybe peek ahead instead, then parse it all at once?
  let new = ByteString.cons start rest
  if
    | Set.member new reservedWords → fail "symbol is reserved word"
    | otherwise → pure (internText (Encoding.decodeUtf8 new))

--------------------------------------------------------------------------------
-- Misc helpers
--------------------------------------------------------------------------------

reservedWords ∷ (Ord a, IsString a) ⇒ Set a
reservedWords =
  Set.fromList ["let", "val", "type", "match", "in", "open", "if", "cond"]

maybe ∷ Alternative f ⇒ f a → f (Maybe a)
maybe = optional

spacer ∷ Parser p → Parser p
spacer p = p <* takeWhile (Lexer.space ==)

spaceLiner ∷ Parser p → Parser p
spaceLiner p = p <* takeWhile (\x → Lexer.space == x || Lexer.endOfLine x)

between ∷ Word8 → Parser p → Word8 → Parser p
between fst p end = spaceLiner (skip (== fst)) *> spaceLiner p <* satisfy (== end)

parens ∷ Parser p → Parser p
parens p = between Lexer.openParen p Lexer.closeParen

curly ∷ Parser p → Parser p
curly p = between Lexer.openCurly p Lexer.closeCurly

many1H ∷ Alternative f ⇒ f a → f (NonEmpty a)
many1H = fmap NonEmpty.fromList . many1

--------------------------------------------------------------------------------
-- SN Derivatives
--------------------------------------------------------------------------------

expressionSN = spaceLiner expression

expressionS = spacer expression

typePSN ∷ Parser Types.Type
typePSN = spaceLiner typeP

typePS ∷ Parser Types.Type
typePS = spacer typeP

typeSumParserSN ∷ Parser Types.TypeSum
typeSumParserSN = spaceLiner typeSumParser

typeSumParserS ∷ Parser Types.TypeSum
typeSumParserS = spacer typeSumParser

recordSN ∷ Parser Types.Record
recordSN = spaceLiner record

recordS ∷ Parser Types.Record
recordS = spacer record

nameParserColonSN ∷ Parser Types.Name
nameParserColonSN = spaceLiner nameParserColon

nameParserSN ∷ Parser Types.Name
nameParserSN = spaceLiner nameParser

arrowTypeSN ∷ Parser Types.ArrowType
arrowTypeSN = spaceLiner arrowType

arrowTypeS ∷ Parser Types.ArrowType
arrowTypeS = spacer arrowType

typeRefineSN ∷ Parser Types.TypeRefine
typeRefineSN = spaceLiner typeRefine

typeRefineS ∷ Parser Types.TypeRefine
typeRefineS = spacer typeRefine

sumSN ∷ Parser Types.Sum
sumSN = spaceLiner sum

sumS ∷ Parser Types.Sum
sumS = spaceLiner sum

arrowsSN ∷ Parser Types.ArrowData
arrowsSN = spaceLiner arrows

parendArrowSN ∷ Parser Types.ArrowParen
parendArrowSN = spaceLiner parendArrow

typeNameParserSN ∷ Parser Types.TypeName
typeNameParserSN = spaceLiner typeNameParser

typeNameParserS ∷ Parser Types.TypeName
typeNameParserS = spacer typeNameParser

prefixSymbolSN ∷ Parser Symbol
prefixSymbolSN = spaceLiner prefixSymbol

prefixSymbolS ∷ Parser Symbol
prefixSymbolS = spacer prefixSymbol
