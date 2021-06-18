{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Juvix.Library.Usage
  ( T (..),
    numToNat,
    allows,
    pred,
    minus,
  )
where

import Juvix.Library hiding (pred, show)
import qualified Juvix.Library.PrettyPrint as PP

-- | Each binder and local context element in Juvix is annotated with a
-- /usage/, which tracks how many times it is needed in a runtime-relevant way.
-- In our case, a usage is either a natural number specifying an exact usage
-- (i.e., not an upper bound), or 𝛚, meaning that usage is not tracked for that
-- variable and any number of usages is allowed.
data T
  = -- | Definite usage.
    SNat Natural
  | -- | Any number of usages allowed.
    Omega
  deriving (Eq, Show, Read, Generic, Data, NFData)

-- | Addition
instance Semigroup T where
  SNat π <> SNat ρ = SNat (π + ρ)
  Omega <> _ = Omega
  _ <> Omega = Omega

instance Monoid T where
  mempty = SNat 0

-- | Multiplication
instance Semiring T where
  one = SNat 1

  SNat π <.> SNat ρ = SNat (π * ρ)
  Omega <.> _ = Omega
  _ <.> Omega = Omega

type instance PP.Ann T = ()

instance PP.PrettySyntax T where
  pretty' (SNat π) = pure $ PP.show π
  pretty' Omega = pure "ω"

pred :: T -> T
pred (SNat x) = SNat (x - 1)
pred Omega = Omega

minus :: T -> T -> Maybe T
minus Omega _ = Just Omega
minus (SNat π) (SNat ρ) | π >= ρ = Just $ SNat $ π - ρ
minus _ _ = Nothing

infixl 6 `minus` -- same as -

-- | Converts an integer to a usage.
numToNat :: Integer -> T
numToNat = SNat . fromInteger

-- variables annotated with n can be used n times.
-- variables annotated with Omega can be used any times.

-- | allows is the function that checks usage compatibility
allows :: T -> T -> Bool
allows (SNat x) (SNat y) = x == y
allows Omega (SNat _) = True
allows Omega Omega = True
allows (SNat _) Omega = False

infix 4 `allows` -- same as <=
