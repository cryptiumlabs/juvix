module Juvix.Core.Usage (Usage, NatAndw (..), numToNat, allowsUsageOf) where

import Juvix.Library hiding (show)
import Prelude (Show (..))

-- | Usage is an alias for the semiring representation
type Usage = NatAndw

-- | NatAndw is the choice of the semiring for ({ℕ, ω}, (+), 0, (*), 1)
data NatAndw
  = -- | semiring of (Nat,w) for usage annotation
    -- 0, 1, or n usage
    SNat Natural
  | -- | unspecified usage
    Omega
  deriving (Eq)

instance Show NatAndw where
  show (SNat n) = show n
  show Omega = "w"

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

-- | numToNat is a helper function that converts an integer to NatAndW
numToNat ∷ Integer → NatAndw
numToNat = SNat . fromInteger

-- | allowsUsageOf is the function that checks usage compatibility
allowsUsageOf ∷ Usage → Usage → Bool
allowsUsageOf (SNat x) (SNat y) = x == y --variables annotated with n can be used n times.
allowsUsageOf Omega (SNat _) = True -- variables annotated with Omega can be used any times.
allowsUsageOf Omega Omega = True
allowsUsageOf (SNat _) Omega = False
