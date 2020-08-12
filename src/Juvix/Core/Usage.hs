{-# LANGUAGE DeriveAnyClass #-}

module Juvix.Core.Usage
  ( Usage,
    NatAndw (..),
    T,
    numToNat,
    allowsUsageOf,
    allows,
    pred,
    minus,
  )
where

import Juvix.Library hiding (pred, show)
import qualified Text.Read as R
import Prelude (Show (..))

-- | Usage is an alias for the semiring representation
type T = NatAndw

type Usage = NatAndw

-- | NatAndw is the choice of the semiring for ({ℕ, ω}, (+), 0, (*), 1)
data NatAndw
  = -- | semiring of (Nat,w) for usage annotation
    -- 0, 1, or n usage
    SNat Natural
  | -- | unspecified usage
    Omega
  deriving (Eq, Generic, NFData)

instance Show NatAndw where
  show (SNat n) = show n
  show Omega = "w"

instance Read NatAndw where
  readPrec =
    R.parens $
      ( R.prec 10 $ do
          R.Symbol "w" <- R.lexP
          return Omega
      )
        R.+++ (R.prec 10 $ SNat <$> R.readPrec)
  readListPrec = R.readListPrecDefault

-- Addition is the semi-Ring/Monoid instance
instance Semigroup NatAndw where
  SNat x <> SNat y = SNat (x + y)
  Omega <> _ = Omega
  _ <> Omega = Omega

instance Monoid NatAndw where
  mempty = SNat 0

-- Semiring instance is thus multiplication
instance Semiring NatAndw where
  one = SNat 1

  SNat x <.> SNat y = SNat (x * y)
  Omega <.> _ = Omega
  _ <.> Omega = Omega

pred :: NatAndw -> NatAndw
pred (SNat x) = SNat (x - 1)
pred Omega = Omega

minus :: Usage -> Usage -> Maybe Usage
minus Omega _ = Just Omega
minus (SNat i) (SNat j) | i >= j = Just $ SNat $ i - j
minus _ _ = Nothing

infixl 6 `minus` -- same as -

-- | numToNat is a helper function that converts an integer to NatAndW
numToNat :: Integer -> NatAndw
numToNat = SNat . fromInteger

-- variables annotated with n can be used n times.
-- variables annotated with Omega can be used any times.

-- | allowsUsageOf is the function that checks usage compatibility
allowsUsageOf :: Usage -> Usage -> Bool
allowsUsageOf (SNat x) (SNat y) = x == y
allowsUsageOf Omega (SNat _) = True
allowsUsageOf Omega Omega = True
allowsUsageOf (SNat _) Omega = False

allows :: Usage -> Usage -> Bool
allows = allowsUsageOf

infix 4 `allowsUsageOf`, `allows` -- same as <=
