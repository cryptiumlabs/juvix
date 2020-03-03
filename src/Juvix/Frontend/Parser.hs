{-# LANGUAGE ApplicativeDo #-}

-- |
-- - The front end parser for the Juvix Programming language
--
-- - Parsers with S at the end, eat the spaces at the end of the parse
module Juvix.Frontend.Parser where

import Data.Attoparsec.ByteString
import qualified Data.ByteString as ByteString
import qualified Data.Char as Char
import qualified Data.Text.Encoding as Encoding
import qualified GHC.Unicode as Unicode
import qualified Juvix.Frontend.Types as Types
import Juvix.Library hiding (maybe, option, takeWhile)

--------------------------------------------------------------------------------
-- Top Level
--------------------------------------------------------------------------------

topLevel = typeP <|> function

expressionS = spacer expression

expression = undefined

usage = string "u#" *> expression

function = undefined

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

typePS ∷ Parser Types.Type
typePS = spacer typeP

typeP ∷ Parser Types.Type
typeP = do
  string "type"
  usag ← maybe usage
  name ← prefixSymbolS
  args ← many prefixSymbolS
  form ← typeSumParser
  pure (Types.Typ usag name args form)

typeSumParser =
  aliasParser
    <|> newTypeParser
    <|> dataParser

aliasParser = undefined

newTypeParser = undefined

dataParser = undefined

--------------------------------------------------
-- TypeNameParser and typeRefine Parser
--------------------------------------------------

typeRefine ∷ Parser Types.TypeRefine
typeRefine = do
  typeName ← typeNameParserS
  refine ← maybe (curly expressionS)
  pure (Types.TypeRefine typeName refine)

typeNameParserS ∷ Parser Types.TypeName
typeNameParserS = spacer typeNameParser

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


prefixSymbolS ∷ Parser Symbol
prefixSymbolS = spacer prefixSymbol

prefixSymbol ∷ Parser Symbol
prefixSymbol = do
  start ← satisfy validStartSymbol
  rest ← takeWhile validMiddleSymbol
  -- Slow O(n) call, could maybe peek ahead instead, then parse it all at once?
  let new = ByteString.cons start rest
  pure (internText (Encoding.decodeUtf8 new))

--------------------------------------------------------------------------------
-- Misc helpers
--------------------------------------------------------------------------------

maybe ∷ Alternative f ⇒ f a → f (Maybe a)
maybe = optional

spacer ∷ Parser p → Parser p
spacer p = p <* takeWhile (space ==)

between ∷ Word8 → Parser p → Word8 → Parser p
between fst p end = satisfy (== fst) *> p <* satisfy (== end)

parens ∷ Parser p → Parser p
parens p = between openParen p closeParen

curly ∷ Parser p → Parser p
curly p = between openCurly  p closeCurly

--------------------------------------------------------------------------------
-- Lexer
--------------------------------------------------------------------------------

wordToChr ∷ Integral a ⇒ a → Char
wordToChr = Char.chr . fromIntegral

-- Hopefully this is fast!
validStartSymbol ∷ Integral a ⇒ a → Bool
validStartSymbol = Unicode.isAlpha . wordToChr

dash ∷ Word8
dash = 45

under ∷ Word8
under = 95

space ∷ Word8
space = 32

openParen ∷ Word8
openParen = 40

closeParen ∷ Word8
closeParen = 40

openCurly ∷ Word8
openCurly = 123

closeCurly ∷ Word8
closeCurly = 125

validMiddleSymbol ∷ Word8 → Bool
validMiddleSymbol w =
  w == dash || Unicode.isAlphaNum (wordToChr w) || w == under

-- check for \r or \n
endOfLine ∷ (Eq a, Num a) ⇒ a → Bool
endOfLine w = w == 13 || w == 10
