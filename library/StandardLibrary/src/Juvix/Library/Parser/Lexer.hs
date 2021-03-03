module Juvix.Library.Parser.Lexer
  ( spacer,
    spaceLiner,
    skipLiner,
    skipLinerS,
    parens,
    brackets,
    curly,
    many1H,
    sepBy1H,
    sepBy1HFinal,
    maybeParend,
    digit,
    isHSpace,
  )
where

import Data.Char (isSpace)
import qualified Data.List.NonEmpty as NonEmpty
import Juvix.Library.Parser.Internal (Parser)
import qualified Juvix.Library.Parser.Token as T
import Protolude
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P

-- | Is it a horizontal space character?
isHSpace :: Char -> Bool
isHSpace x = isSpace x && x /= '\n' && x /= '\r'

digit :: (Ord a, Num a) => a -> Bool
digit w = w <= 57 && w >= 48

spacer :: Parser p -> Parser p
spacer p = P.takeWhileP (Just "spacer") isSpace *> p

spaceLiner :: Parser p -> Parser p
spaceLiner p = P.takeWhileP (Just "space liner") isHSpace *> p

between :: Char -> Parser p -> Char -> Parser p
between fst p end = skipLiner fst *> spaceLiner p <* P.satisfy (== end)

parens :: Parser p -> Parser p
parens p = between T.openParen p T.closeParen

brackets :: Parser p -> Parser p
brackets p = between T.openBracket p T.closeBracket

curly :: Parser p -> Parser p
curly p = between T.openCurly p T.closeCurly

many1H :: Parser a -> Parser (NonEmpty a)
many1H = fmap NonEmpty.fromList . P.some

-- | 'sepBy1HFinal' is like 'sepBy1H' but also tries to parse a last separator
sepBy1HFinal :: Parser a -> Parser s -> Parser (NonEmpty a)
sepBy1HFinal parse sep = sepBy1H parse sep <* P.optional sep

sepBy1H :: Parser a -> Parser s -> Parser (NonEmpty a)
sepBy1H parse sep = NonEmpty.fromList <$> P.sepBy1 parse sep

skipLiner :: Char -> Parser ()
skipLiner p = spaceLiner (P.skipMany (P.char p))

skipLinerS :: Text -> Parser ()
skipLinerS p = spaceLiner (P.skipMany (P.string p))

maybeParend :: Parser a -> Parser a
maybeParend p = p <|> parens p
