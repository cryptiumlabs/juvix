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
import qualified Data.Text.Encoding as Encoding
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
  let nonOverlappingCase =
        notWord8 Lexer.pipe *> pure ()
          <|> notWord8 Lexer.dash *> pure ()
          <|> endOfInput
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
record = undefined

--------------------------------------------------
-- Arrow Type parser
--------------------------------------------------

arrowType ∷ Parser Types.ArrowType
arrowType = undefined

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
  let toType (Symb s) = Types.Next s
      toType (Universe u) = Types.Universe u
  p ← prefixSymbolSN
  other ← many (spaceLiner (universeSymbol <|> Symb <$> prefixSymbolSN))
  pure (foldr toType (Types.Final p) other)

data SymbOrUni = Symb Symbol | Universe Types.UniverseExpression

universeSymbol ∷ Parser SymbOrUni
universeSymbol = do
  _ ← string "u#"
  Universe <$> universeExpression

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
  pure (internText (Encoding.decodeUtf8 new))

--------------------------------------------------------------------------------
-- Misc helpers
--------------------------------------------------------------------------------

maybe ∷ Alternative f ⇒ f a → f (Maybe a)
maybe = optional

spacer ∷ Parser p → Parser p
spacer p = p <* takeWhile (Lexer.space ==)

spaceLiner ∷ Parser p → Parser p
spaceLiner p = p <* takeWhile (\x → Lexer.space == x || Lexer.endOfLine x)

between ∷ Word8 → Parser p → Word8 → Parser p
between fst p end = satisfy (== fst) *> p <* satisfy (== end)

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

typeNameParserSN ∷ Parser Types.TypeName
typeNameParserSN = spaceLiner typeNameParser

typeNameParserS ∷ Parser Types.TypeName
typeNameParserS = spacer typeNameParser

prefixSymbolSN ∷ Parser Symbol
prefixSymbolSN = spaceLiner prefixSymbol

prefixSymbolS ∷ Parser Symbol
prefixSymbolS = spacer prefixSymbol
