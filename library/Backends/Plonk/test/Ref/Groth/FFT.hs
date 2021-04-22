{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- | Discrete Fourier transforms for polynomial interpolation
module Ref.Groth.FFT
  ( CoeffVec,
    dftNaive,
    fft,
    fftMult,
    fftTargetPoly,
    interpolate,
    inverseDft,
  )
where

import Data.Field.Galois (GaloisField, pow)
import qualified Data.List as List
import Data.Poly (VPoly, monomial, toPoly)
import Data.Vector (fromList)
import Juvix.Library

-- | Polynomial represented as a coefficient vector, little-endian
type CoeffVec f = [f]

-- | Discrete Fourier transform. Can be interpreted as some polynomial
-- evaluated at certain roots of unity. (In our case the length of
-- these lists will be a power of two.)
type DFT f = [f]

-- | Evaluate a polynomial given by its coefficient vector
evalPoly :: Num f => CoeffVec f -> f -> f
evalPoly coeffs x = foldr (\c rest -> c + x * rest) 0 coeffs

-- | Naive discrete Fourier transformation performed by evaluating the
-- polynomial at the appropriate roots of unity.
dftNaive ::
  Num f =>
  -- | principal 2^k-th root of unity
  f ->
  -- | polynomial coefficients, length should be 2^k for
  -- some k
  CoeffVec f ->
  DFT f
dftNaive omega_n as = map (\i -> evalPoly as (omega_n ^ i)) [0 .. length as - 1]

-- | Split a list into a list containing the odd-numbered and one with
-- the even-numbered elements.
split :: [a] -> ([a], [a])
split = foldr (\a (r1, r2) -> (a : r2, r1)) ([], [])

-- | Calculate ceiling of log base 2 of an integer.
log2 :: Int -> Int
log2 x = floorLog + correction
  where
    floorLog = finiteBitSize x - 1 - countLeadingZeros x
    correction =
      if countTrailingZeros x < floorLog
        then 1
        else 0

-- | Fast Fourier transformation.
fft ::
  GaloisField k =>
  -- | function that gives for input n the principal (2^n)-th root of unity
  (Int -> k) ->
  -- | length should be n
  CoeffVec k ->
  DFT k
fft omega_n as =
  case length as of
    1 -> as
    n ->
      let (as0, as1) = split as
          y0 = fft omega_n as0
          y1 = fft omega_n as1
          omegas = map (pow (omega_n (log2 n))) [0 .. n]
       in combine y0 y1 omegas
  where
    combine y0 y1 omegas =
      (\xs -> map fst xs ++ map snd xs) $
        map (\(yk0, yk1, currentOmega) -> (yk0 + currentOmega * yk1, yk0 - currentOmega * yk1)) $
          List.zip3 y0 y1 omegas

-- | Inverse discrete Fourier transformation, uses FFT.
inverseDft :: GaloisField k => (Int -> k) -> DFT k -> CoeffVec k
inverseDft primRootsUnity dft =
  let n = fromIntegral . length $ dft
   in map (/ n) $
        fft (recip . primRootsUnity) dft

-- | Append minimal amount of zeroes until the list has a length which
-- is a power of two.
padToNearestPowerOfTwo :: Num f => [f] -> [f]
padToNearestPowerOfTwo [] = []
padToNearestPowerOfTwo xs = padToNearestPowerOfTwoOf (length xs) xs

-- | Given n, append zeroes until the list has length 2^n.
padToNearestPowerOfTwoOf ::
  Num f =>
  -- | n
  Int ->
  -- | list which should have length <= 2^n
  [f] ->
  -- | list which will have length 2^n
  [f]
padToNearestPowerOfTwoOf i xs = xs ++ replicate padLength 0
  where
    padLength = nearestPowerOfTwo - length xs
    nearestPowerOfTwo = bit $ log2 i

-- | Create a polynomial that goes through the given values.
interpolate :: GaloisField k => (Int -> k) -> [k] -> VPoly k
interpolate primRoots pts = toPoly . fromList $ inverseDft primRoots (padToNearestPowerOfTwo pts)

-- | Multiply polynomials using FFT
fftMult :: GaloisField k => (Int -> k) -> CoeffVec k -> CoeffVec k -> CoeffVec k
fftMult primRoots l r = inverseDft primRoots $ zipWith (*) dftL dftR
  where
    n = 2 * max (length l) (length r)
    paddedDft x = fft primRoots (padToNearestPowerOfTwoOf n x)
    dftL = paddedDft l
    dftR = paddedDft r

-- XXX make this actually go fast
-- polyWithZeroesAt
--    :: Fractional f
--    => (Int -> f)
--    -> [f]
--    -> CoeffVec f
-- polyWithZeroesAt primRoots
--   = foldl' (fftMult primRoots) [1]
--     . map (\xcoord -> [-xcoord, 1])

-- XXX make this actually use FFT mult
fftTargetPoly :: GaloisField k => (Int -> k) -> Int -> VPoly k
fftTargetPoly primRoots numRoots =
  foldl' (*) (monomial 0 1) ((\i -> toPoly . fromList $ [- pow omega i, 1]) <$> [0 .. 2 ^ k - 1 :: Integer])
  where
    k = log2 numRoots
    omega = primRoots k
