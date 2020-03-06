module Juvix.Frontend.Lexer where

import qualified Data.Char as Char
import qualified GHC.Unicode as Unicode
import Juvix.Library hiding (maybe, option, takeWhile)

wordToChr ∷ Integral a ⇒ a → Char
wordToChr = Char.chr . fromIntegral

-- Hopefully this is fast!
validStartSymbol ∷ Integral a ⇒ a → Bool
validStartSymbol = Unicode.isAlpha . wordToChr

-- Unicode.isUpper 'İ' = True!
validUpperSymbol ∷ Integral a ⇒ a → Bool
validUpperSymbol = Unicode.isUpper . wordToChr

dash ∷ Word8
dash = 45

under ∷ Word8
under = 95

space ∷ Word8
space = 32

colon ∷ Word8
colon = 58

comma ∷ Word8
comma = 44

hash ∷ Word8
hash = 35

openParen ∷ Word8
openParen = 40

closeParen ∷ Word8
closeParen = 41

openCurly ∷ Word8
openCurly = 123

pipe ∷ Word8
pipe = 124

closeCurly ∷ Word8
closeCurly = 125

equals ∷ Word8
equals = 61

at ∷ Word8
at = 64

validMiddleSymbol ∷ Word8 → Bool
validMiddleSymbol w =
  w == dash || Unicode.isAlphaNum (wordToChr w) || w == under

-- check for \r or \n
endOfLine ∷ (Eq a, Num a) ⇒ a → Bool
endOfLine w = w == 13 || w == 10
