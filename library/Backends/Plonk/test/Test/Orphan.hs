{-# LANGUAGE DeriveAnyClass #-}

module Test.Orphan where

import qualified Data.Aeson as A
import Data.Curve.Weierstrass.BLS12381 (Fr)
import Data.Field.Galois (GaloisField, PrimeField (..), toP)
import qualified Data.Scientific as S
import Juvix.Library
import Text.Read (Lexeme (..), Read (..), lexP, parens, pfail, step)
import Text.Read.Lex (numberToInteger)

deriving instance Bits Fr

instance A.FromJSON Fr where
  parseJSON (A.Number n) = case S.floatingOrInteger n of
    Left floating -> panic $ "Can't parse floating :" <> show n
    Right f -> pure . toP $ toInteger f

instance A.ToJSON Fr where
  toJSON f = A.Number $ S.scientific (fromP f) 0

instance Read Fr where
  readPrec = parens $ do
    Ident "P" <- step lexP
    Punc "(" <- step lexP
    Number n <- step lexP
    Punc "`" <- step lexP
    Ident "modulo" <- step lexP
    Punc "`" <- step lexP
    Number p <- step lexP
    Punc ")" <- step lexP
    case numberToInteger n of
      Just i -> pure $ toP i
      Nothing -> pfail
