{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

-- A number of definitions missing in the Data.Poly library.

module Ref.Groth.Poly
  ( secretEvalInExponent,
  )
where

import Data.Curve (Curve (..), mul)
import Data.Field.Galois (PrimeField (..))
import Data.Poly (VPoly, unPoly)
import Juvix.Library hiding (quot, rem)

-- | Given a polynomial p(x), evaluate g^(p(s)) at a secret value s,
-- using the powers g^s, g^(s^2), etc., instead of using the secret
-- value directly.
secretEvalInExponent ::
  (PrimeField r, Curve f c e q r) =>
  -- | polynomial p(x) to evaluate in the exponent at some secret point s.
  VPoly r ->
  -- | powers of s (in the exponent of g),
  -- e.g. [g^(s^0), g^(s^1), g^(s^2), ..., g^(s^k)].
  -- Length of this list should be degree of given polynomial.
  [Point f c e q r] ->
  -- | g^(p(s))
  Point f c e q r
secretEvalInExponent p powersOfS =
  fold $ zipWith (flip mul) (toList . unPoly $ p) powersOfS
