module Juvix.Frontend.Lexer where

import qualified GHC.Unicode as Unicode
import Juvix.Library hiding (maybe, option, takeWhile)

charToWord8 :: Char -> Word8
charToWord8 = fromIntegral . ord
{-# INLINE charToWord8 #-}

wordToChr :: Integral a => a -> Char
wordToChr = chr . fromIntegral

-- Hopefully this is fast!
validStartSymbol' :: Integral a => a -> Bool
validStartSymbol' = Unicode.isAlpha . wordToChr

-- Unicode.isUpper 'İ' = True!
validUpperSymbol :: Integral a => a -> Bool
validUpperSymbol = Unicode.isUpper . wordToChr

dash :: Word8
dash = charToWord8 '-'

under :: Word8
under = charToWord8 '_'

space :: Word8
space = charToWord8 ' '

colon :: Word8
colon = charToWord8 ':'

semi :: Word8
semi = charToWord8 ';'

comma :: Word8
comma = charToWord8 ','

hash :: Word8
hash = charToWord8 '#'

openParen :: Word8
openParen = charToWord8 '('

closeParen :: Word8
closeParen = charToWord8 ')'

backSlash :: Word8
backSlash = charToWord8 '\\'

quote :: Word8
quote = charToWord8 '\''

openCurly :: Word8
openCurly = charToWord8 '{'

pipe :: Word8
pipe = charToWord8 '|'

closeCurly :: Word8
closeCurly = charToWord8 '}'

equals :: Word8
equals = charToWord8 '='

at :: Word8
at = charToWord8 '@'

dot :: Word8
dot = charToWord8 '.'

amper :: Word8
amper = charToWord8 '&'

times :: Word8
times = charToWord8 '*'

backtick :: Word8
backtick = charToWord8 '`'

validStartSymbol :: Word8 -> Bool
validStartSymbol w =
  validStartSymbol' w || w == under

validInfixSymbol :: Word8 -> Bool
validInfixSymbol w =
  Unicode.isSymbol (wordToChr w) || w == times || w == dash || w == amper || w == colon

validMiddleSymbol :: Word8 -> Bool
validMiddleSymbol w =
  w == dash || validStartSymbol w

-- check for \r or \n
endOfLine :: (Eq a, Num a) => a -> Bool
endOfLine w = w == 13 || w == 10

digit :: (Ord a, Num a) => a -> Bool
digit w = w <= 57 && w >= 48
