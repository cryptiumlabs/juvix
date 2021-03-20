{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Notation in this module follows
-- On the Size of Pairing-based Non-interactive Arguments
-- https://eprint.iacr.org/2016/260.pdf

module Ref.Groth
  ( RandomSetup (..),
    RandomProver (..),
    RandomSimulator (..),
    Trapdoor (..),
    Proof (..),
    generateRandomSetup,
    generateRandomProver,
    generateRandomSimulator,
    simulate,
    Reference (..),
    RefP (..),
    RefV (..),
    setup,
    prove,
    verify,
  )
where

import Data.Curve.Weierstrass (gen, inv, mul)
import Data.Euclidean (Euclidean (..))
import Data.Field.Galois (pow)
import qualified Data.Map as Map
import Data.Pairing.BLS12381 (BLS12381, Fr, Pairing (..))
import Data.Poly (VPoly, eval, monomial)
import Juvix.Backends.Plonk.Circuit (ArithCircuit (..), Wire)
import Juvix.Library hiding (quotRem)
import Ref.Groth.Poly (secretEvalInExponent)
import Ref.Groth.QAP
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Arbitrary.Generic (genericArbitrary)
import Text.PrettyPrint.Leijen.Text hiding ((<$>))

-- | Random values in Z_p^* picked in the setup.
data RandomSetup f
  = RandomSetup -- alpha, beta, gamma, delta, x <- Z_p^*
      { setupAlpha :: f, -- alpha
        setupBeta :: f, -- beta
        setupGamma :: f, -- gamma
        setupDelta :: f, -- delta
        setupX :: f -- x
      }
  deriving (Show, Generic, NFData)

instance Pretty f => Pretty (RandomSetup f) where
  pretty RandomSetup {..} =
    vcat
      [ "alpha : " <> pretty setupAlpha,
        "beta  : " <> pretty setupBeta,
        "gamma : " <> pretty setupGamma,
        "delta : " <> pretty setupDelta,
        "x     : " <> pretty setupX
      ]

instance Arbitrary f => Arbitrary (RandomSetup f) where
  arbitrary = genericArbitrary

-- | Random values in Z_p picked in the prover algorithm.
data RandomProver f
  = RandomProver -- r, s <- Z_p
      { proveR :: f, -- r
        proveS :: f -- s
      }
  deriving (Show, Generic, NFData)

instance Pretty f => Pretty (RandomProver f) where
  pretty RandomProver {..} =
    vcat
      [ "r : " <> pretty proveR,
        "s : " <> pretty proveS
      ]

instance Arbitrary f => Arbitrary (RandomProver f) where
  arbitrary = genericArbitrary

-- | Random values in Z_p picked in the simulator.
data RandomSimulator f
  = RandomSimulator -- A, B <- Z_p
      { simulateA :: f, -- A
        simulateB :: f -- B
      }
  deriving (Show, Generic, NFData)

instance Pretty f => Pretty (RandomSimulator f) where
  pretty RandomSimulator {..} =
    vcat
      [ "A : " <> pretty simulateA,
        "B : " <> pretty simulateB
      ]

instance Arbitrary f => Arbitrary (RandomSimulator f) where
  arbitrary = genericArbitrary

-- | Common reference string produced in the setup.
data Reference g1 g2
  = Reference -- sigma = sigma_r + sigma_v
      { refP :: RefP g1 g2, -- sigma_r
        refV :: RefV g1 g2 -- sigma_v
      }
  deriving (Show, Generic, NFData)

-- | Common reference string in the prover algorithm.
data RefP g1 g2
  = RefP -- sigma_r <- sigma
      { refP1Alpha :: g1, -- [alpha]_1
        refP1Beta :: g1, -- [beta]_1
        refP1Delta :: g1, -- [delta]_1
        refP1Z :: QapSet g1, -- {[(beta*u_i(x) + alpha*v_i(x) + w_i(x))/delta]_1}_(l+1)^m
        refP1T :: [g1], -- {[x^i*t(x)/delta]_1}_0^(n-2)
        refP1U :: QapSet g1, -- {[u_i(x)]_1}_0^m
        refP1V :: QapSet g1, -- {[v_i(x)]_1}_0^m
        refP2Beta :: g2, -- [beta]_2
        refP2Gamma :: g2, -- [gamma]_2
        refP2Delta :: g2, -- [delta]_2
        refP2V :: QapSet g2 -- {[v_i(x)]_2}_0^m
      }
  deriving (Show, Generic, NFData)

instance (Pretty g1, Pretty g2) => Pretty (RefP g1 g2) where
  pretty RefP {..} =
    vcat
      [ text "[alpha]_1 : ",
        indent 2 $ pretty refP1Alpha,
        text "[beta]_1  : ",
        indent 2 $ pretty refP1Beta,
        text "[delta]_1 : ",
        indent 2 $ pretty refP1Delta,
        text "[Z_i]_1   : ",
        indent 2 $ ppQapSet refP1Z,
        text "[T_i]_1   : ",
        indent 2 $ pretty refP1T,
        text "[beta]_2  : ",
        indent 2 $ pretty refP2Beta,
        text "[gamma]_2 : ",
        indent 2 $ pretty refP2Gamma,
        text "[delta]_2 : ",
        indent 2 $ pretty refP2Delta
      ]
    where
      ppQapSet =
        vcat
          . map (\(ix, x) -> enclose lbracket rbracket (pretty ix) <+> pretty x)
          . Map.toList
          . qapSetToMap

-- | Common reference string in the verification algorithm.
data RefV g1 g2
  = RefV -- sigma_v <- sigma
      { refVY :: QapSet g1, -- {[(beta*u_i(x) + alpha*v_i(x) + w_i(x))/gamma]_1}_0^l
        refVAlpha :: g1, -- [alpha]_1
        refVBeta :: g2, -- [beta]_2
        refVGamma :: g2, -- [gamma]_2
        refVDelta :: g2 -- [delta]_2
      }
  deriving (Show, Generic, NFData)

instance (Pretty g1, Pretty g2) => Pretty (RefV g1 g2) where
  pretty RefV {..} =
    vcat
      [ text "[Y_i]_1   : ",
        indent 2 $ ppQapSet refVY,
        text "[alpha]_1 : ",
        indent 2 $ pretty refVAlpha,
        text "[beta]_1  : ",
        indent 2 $ pretty refVBeta,
        text "[gamma]_2 : ",
        indent 2 $ pretty refVGamma,
        text "[delta]_2 : ",
        indent 2 $ pretty refVDelta
      ]
    where
      ppQapSet =
        vcat
          . map (\(ix, x) -> enclose lbracket rbracket (pretty ix) <+> pretty x)
          . Map.toList
          . qapSetToMap

-- | Simulation trapdoor produced in the setup.
data Trapdoor f
  = Trapdoor -- tau = (alpha, beta, gamma, delta, x)
      { trapdoorAlpha :: f, -- alpha
        trapdoorBeta :: f, -- beta
        trapdoorGamma :: f, -- gamma
        trapdoorDelta :: f, -- delta
        trapdoorX :: f -- x
      }
  deriving (Show, Generic, NFData)

instance Pretty f => Pretty (Trapdoor f) where
  pretty Trapdoor {..} =
    vcat
      [ "alpha : " <> pretty trapdoorAlpha,
        "beta  : " <> pretty trapdoorBeta,
        "gamma : " <> pretty trapdoorGamma,
        "delta : " <> pretty trapdoorDelta,
        "x     : " <> pretty trapdoorX
      ]

-- | Proof returned in the prover algorithm.
data Proof g1 g2
  = Proof -- pi = ([A]_1, [B]_2, [C]_1)
      { proofA :: g1, -- [A]_1
        proofB :: g2, -- [B]_2
        proofC :: g1 -- [C]_1
      }
  deriving (Show, Generic, NFData)

instance (Pretty g1, Pretty g2) => Pretty (Proof g1 g2) where
  pretty Proof {..} =
    vcat
      [ text "[A]_1 : ",
        indent 2 $ pretty proofA,
        text "[B]_2 : ",
        indent 2 $ pretty proofB,
        text "[C]_1 : ",
        indent 2 $ pretty proofC
      ]

-- | Generate random values in Z_p^* for the setup. (WIP: non zero)
generateRandomSetup :: Monad m => m f -> m (RandomSetup f)
generateRandomSetup r = RandomSetup <$> r <*> r <*> r <*> r <*> r

-- | Generate random values in Z_p for the prover algorithm.
generateRandomProver :: Monad m => m f -> m (RandomProver f)
generateRandomProver r = RandomProver <$> r <*> r

-- | Generate random values in Z_p for the simulator.
generateRandomSimulator :: Monad m => m f -> m (RandomSimulator f)
generateRandomSimulator r = RandomSimulator <$> r <*> r

-- | The setup produces a common reference string and a simulation trapdoor for
-- a quadratic integer program.
setup ::
  RandomSetup Fr ->
  QAP Fr ->
  (Reference (G1 BLS12381) (G2 BLS12381), Trapdoor Fr)
setup RandomSetup {..} QAP {..} =
  ( Reference
      { refP =
          RefP
            { refP1Alpha = mul gen setupAlpha,
              refP1Beta = mul gen setupBeta,
              refP1Delta = mul gen setupDelta,
              refP1Z = mul gen <$> z,
              refP1T = mul gen <$> xt,
              refP1U = mul gen <$> ux,
              refP1V = mul gen <$> vx,
              refP2Beta = mul gen setupBeta,
              refP2Gamma = mul gen setupGamma,
              refP2Delta = mul gen setupDelta,
              refP2V = mul gen <$> vx
            },
        refV =
          RefV
            { refVY = mul gen <$> y,
              refVAlpha = mul gen setupAlpha,
              refVBeta = mul gen setupBeta,
              refVGamma = mul gen setupGamma,
              refVDelta = mul gen setupDelta
            }
      },
    Trapdoor setupAlpha setupBeta setupGamma setupDelta setupX
  )
  where
    combineInputsQap, combineNonInputsQap :: QapSet Fr -> QapSet Fr -> QapSet Fr
    combineInputsQap = combineInputsWithDefaults (+) 0 0
    combineNonInputsQap = combineNonInputsWithDefaults (+) 0 0 0
    -- {[u_i(x)]_1}_0^m
    ux :: QapSet Fr
    ux = flip eval setupX <$> qapInputsLeft
    -- {[v_i(x)]_1}_0^m
    vx :: QapSet Fr
    vx = flip eval setupX <$> qapInputsRight
    -- Separate between inputs and (intermediates + outputs)
    -- {[beta*u_i(x)]_1}_0^l, {[beta*u_i(x)]_1}_(l+1)^m
    uY :: QapSet Fr
    uY = (* setupBeta) <$> ux
    -- {[alpha*v_i(x)]_1}_0^l, {[alpha*v_i(x)]_1}_(l+1)^m
    vY :: QapSet Fr
    vY = (* setupAlpha) <$> vx
    -- {[w_i(x)]_1}_0^l, {[w_i(x)]_1}_(l+1)^m
    wY :: QapSet Fr
    wY = flip eval setupX <$> qapOutputs
    -- {[(beta*u_i(x) + alpha*v_i(x) + w_i(x))/gamma]_1}_0^l
    -- l is meant to be the number of inputs, so wY is not required, as it will be 0.
    y :: QapSet Fr
    y = (/ setupGamma) <$> uY `combineInputsQap` vY
    -- {[(beta*u_i(x) + alpha*v_i(x) + w_i(x))/delta]_1}_(l+1)^m
    z :: QapSet Fr
    z = (/ setupDelta) <$> uY `combineNonInputsQap` vY `combineNonInputsQap` wY
    -- {[x^i*t(x)/delta]_1}_0^(n-2)
    xt :: [Fr]
    xt = (/ setupDelta) . (* eval qapTarget setupX) . pow setupX <$> [0 .. degree qapTarget]

-- | The prover algorithm takes as input a common reference string and a list
-- of a_i, and returns a proof.
prove ::
  RandomProver Fr ->
  ArithCircuit Fr ->
  QAP Fr ->
  RefP (G1 BLS12381) (G2 BLS12381) ->
  Map Wire Fr -> -- Inputs
  Proof (G1 BLS12381) (G2 BLS12381)
prove RandomProver {..} circuit QAP {..} RefP {..} inps = Proof a b c
  where
    as = generateAssignment circuit inps
    -- [alpha]_1 . sum_0^m a_i[U_i]_1 . r[delta]_1
    a :: G1 BLS12381
    a =
      refP1Alpha
        <> fold (combineWithDefaults mul mempty 0 refP1U as)
        <> mul refP1Delta proveR
    -- [beta]_2 . sum_0^m a_i[V_i]_2 . s[delta]_2
    b :: G2 BLS12381
    b = refP2Beta <> fold (combineWithDefaults mul mempty 0 refP2V as) <> mul refP2Delta proveS
    -- sum_(l+1)^m a_i[Z_i]_1
    -- . h([x*t(x)/delta]_1)
    -- . s([alpha]_1 . sum_0^m a_i[U_i]_1 . r[delta]_1)
    -- . r([beta]_1 . sum_0^m a_i[V_i]_1 . s[delta]_1)
    -- . -rs[delta]_1
    c :: G1 BLS12381
    c = az <> ht <> a' <> b' <> inv rsdelta
      where
        az, ht, a', b', rsdelta :: G1 BLS12381
        az = sumQapSetMidOut (combineWithDefaults mul mempty 0 refP1Z as)
        ht = secretEvalInExponent h refP1T
          where
            h :: VPoly Fr
            h = if r == 0 then q else panic "h(X) is undefined."
              where
                q , r :: VPoly Fr
                (q, r) = quotRem (((add qapInputsLeft) * (add qapInputsRight)) - (add qapOutputs)) qapTarget
                  where
                    add :: QapSet (VPoly Fr) -> VPoly Fr
                    add = sum . combineWithDefaults ((*) . monomial 0) 0 0 as
        a' = mul a proveS
        b' = mul b'' proveR
          where
            b'' :: G1 BLS12381
            b'' = refP1Beta <> fold (combineWithDefaults mul mempty 0 refP1V as) <> mul refP1Delta proveS
        rsdelta = mul refP1Delta (proveR * proveS)

-- | The verification algorithm takes as input a restricted common reference
-- string, a list of a_i, and a proof, and returns a rejection or an acceptance.
verify ::
  RefV (G1 BLS12381) (G2 BLS12381) ->
  Map Wire Fr ->
  Proof (G1 BLS12381) (G2 BLS12381) ->
  Bool
verify RefV {..} inps Proof {..} = ab == alphabeta <> aygamma <> cdelta
  where
    -- <[A]_1, [B]_2>
    ab :: GT BLS12381
    ab = pairing proofA proofB
    -- <[alpha]_1, [beta]_2>
    alphabeta :: GT BLS12381
    alphabeta = pairing refVAlpha refVBeta
    -- <sum_0^l a_i[Y_i]_1, [gamma]_2>
    aygamma :: GT BLS12381
    aygamma = pairing (sumQapSetCnstInp (combineWithDefaults mul mempty 0 refVY (cnstInpQapSet 1 inps))) refVGamma
    -- <[C]_1, [delta]_2>
    cdelta :: GT BLS12381
    cdelta = pairing proofC refVDelta

-- | The simulator takes as input a simulation trapdoor and a list of a_i, and
-- returns a proof.
simulate ::
  RandomSimulator Fr ->
  QAP Fr ->
  Trapdoor Fr ->
  Map Wire Fr ->
  Proof (G1 BLS12381) (G2 BLS12381)
simulate RandomSimulator {..} QAP {..} Trapdoor {..} inps = Proof a b c
  where
    -- [A]_1
    a :: G1 BLS12381
    a = mul gen simulateA
    -- [B]_2
    b :: G2 BLS12381
    b = mul gen simulateB
    -- [A*B/delta]_1
    -- . -[alpha*beta/delta]_1
    -- . -sum_0^l a_i[(beta*u_i(x) + alpha*v_i(x) + w_i(x))/delta]_1
    c :: G1 BLS12381
    c = ab <> inv alphabeta <> inv ayz
      where
        combineQap :: QapSet Fr -> QapSet Fr -> QapSet Fr
        combineQap = combineWithDefaults (+) 0 0
        ab, alphabeta, ayz :: G1 BLS12381
        ab = mul gen (simulateA * simulateB / trapdoorDelta)
        alphabeta = mul gen (trapdoorAlpha * trapdoorBeta / trapdoorDelta)
        ayz = sumQapSetCnstInp (combineWithDefaults mul mempty 0 uvw (cnstInpQapSet 1 inps))
          where
            uvw = mul gen . (/ trapdoorDelta) <$> ux `combineQap` vx `combineQap` wx
              where
                ux, vx, wx :: QapSet Fr
                ux = (* trapdoorBeta) . flip eval trapdoorX <$> qapInputsLeft
                vx = (* trapdoorAlpha) . flip eval trapdoorX <$> qapInputsRight
                wx = flip eval trapdoorX <$> qapOutputs
