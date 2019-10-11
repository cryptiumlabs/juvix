module Juvix.Core.SemiRing (Usage, NatAndw (..), numToNat) where

import           Numeric.Natural
import           Juvix.Library   hiding (show)
import           Prelude         (Show (..))

-- | Usage is an alias for the semiring representation
type Usage = NatAndw

-- | NatAndw is the choice of the semiring for ({ℕ, ω}, (+), 0, (*), 1)
data NatAndw     -- semiring of (Nat,w) for usage annotation
  = SNat Natural -- 0, 1, or n usage
  | Omega        -- unspecified usage

instance Show NatAndw where
  show (SNat n) = show n
  show Omega    = "w"

instance Eq NatAndw where
  SNat x == SNat y = x == y
  SNat _ == Omega = True
  Omega == _ = True

-- Addition is the semi-Ring/Monoid instance
instance Semigroup NatAndw where
  SNat x <> SNat y = SNat (x + y)
  Omega  <> _      = Omega
  _      <> Omega  = Omega

instance Monoid NatAndw where
  mempty = SNat 0

-- Semiring instance is thus multiplication
instance Semiring NatAndw where
  one = SNat 1

  SNat x <.> SNat y = SNat (x * y)
  Omega  <.> _      = Omega
  _      <.> Omega  = Omega

-- | numToNat is a helper function that converts an integer to NatAndW
numToNat ∷ Integer → NatAndw
numToNat = SNat . fromInteger
