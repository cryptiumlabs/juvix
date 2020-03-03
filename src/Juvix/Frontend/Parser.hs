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
import qualified Data.Text.Encoding as Encoding
import qualified Juvix.Frontend.Lexer as Lexer
import qualified Juvix.Frontend.Types as Types
import Juvix.Library hiding (maybe, option, takeWhile, try)
import Prelude (fail)

--------------------------------------------------------------------------------
-- Top Level
--------------------------------------------------------------------------------

topLevel = typeP <|> function

expression = undefined

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
  spaceLiner (skip (== Lexer.equals))
  spaceLiner (skip (== Lexer.pipe))
  -- if we get a pipe at the end of this, then we need to go to the other case
  -- Note that we may end up with a non boxed type, but that is fine
  -- this is a subset of the ADT case for analysis
  Types.Declare <$> prefixSymbolSN <*> typeRefineSN <* notWord8 Lexer.pipe

aliasParser ∷ Parser Types.Alias
aliasParser = do
  spaceLiner (skip (== Lexer.equals))
  Types.AliasDec <$> typeRefine

dataParser ∷ Parser Types.Data
dataParser = undefined

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
  p ← prefixSymbolS
  other ← many (spacer (universeSymbol <|> Symb <$> prefixSymbol))
  pure (foldr toType (Types.Final p) other)

data SymbOrUni = Symb Symbol | Universe Types.UniverseExpression

universeSymbol ∷ Parser SymbOrUni
universeSymbol = do
  _ ← string "u#"
  Universe <$> universeExpression

universeExpression ∷ Parser Types.UniverseExpression
universeExpression =
  Types.UniverseExpression <$> prefixSymbol
    -- TODO ∷ make this proper do + and max!
    <|> Types.UniverseExpression <$> parens prefixSymbol

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

typeRefineSN ∷ Parser Types.TypeRefine
typeRefineSN = spaceLiner typeRefine

typeRefineS ∷ Parser Types.TypeRefine
typeRefineS = spacer typeRefine

typeNameParserSN ∷ Parser Types.TypeName
typeNameParserSN = spaceLiner typeNameParser

typeNameParserS ∷ Parser Types.TypeName
typeNameParserS = spacer typeNameParser

prefixSymbolSN ∷ Parser Symbol
prefixSymbolSN = spaceLiner prefixSymbol

prefixSymbolS ∷ Parser Symbol
prefixSymbolS = spacer prefixSymbol
