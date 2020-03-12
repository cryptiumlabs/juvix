module Juvix.Frontend.Lexer where

import qualified Data.Char as Char
import qualified GHC.Unicode as Unicode
import Juvix.Library hiding (maybe, option, takeWhile)

wordToChr ∷ Integral a ⇒ a → Char
wordToChr = Char.chr . fromIntegral

-- Hopefully this is fast!
validStartSymbol' ∷ Integral a ⇒ a → Bool
validStartSymbol' = Unicode.isAlpha . wordToChr

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

semi ∷ Word8
semi = 59

comma ∷ Word8
comma = 44

hash ∷ Word8
hash = 35

openParen ∷ Word8
openParen = 40

closeParen ∷ Word8
closeParen = 41

backSlash ∷ Word8
backSlash = 92

quote ∷ Word8
quote = 34

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

dot ∷ Word8
dot = 46

times ∷ Word8
times = 42

backtick ∷ Word8
backtick = 96

validStartSymbol ∷ Word8 → Bool
validStartSymbol w =
  validStartSymbol' w || w == under

validInfixSymbol ∷ Word8 → Bool
validInfixSymbol w =
  Unicode.isSymbol (wordToChr w) || w == times

validMiddleSymbol ∷ Word8 → Bool
validMiddleSymbol w =
  w == dash || validStartSymbol w

-- check for \r or \n
endOfLine ∷ (Eq a, Num a) ⇒ a → Bool
endOfLine w = w == 13 || w == 10

digit ∷ (Ord a, Num a) ⇒ a → Bool
digit w = w <= 57 && w >= 48
