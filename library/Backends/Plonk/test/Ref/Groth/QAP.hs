{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

{-# OPTIONS -fno-warn-orphans #-}

-- | Definitions of quadratic arithmetic programs, along with their
-- assignment verification functions and the translations from single
-- multiplication- or equality-gates into QAPs and arithmetic circuits
-- into QAPs.
module Ref.Groth.QAP
  ( QapSet (..),
    QAP (..),
    updateAtWire,
    lookupAtWire,
    cnstInpQapSet,
    sumQapSet,
    sumQapSetCnstInp,
    sumQapSetMidOut,
    foldQapSet,
    combineWithDefaults,
    combineInputsWithDefaults,
    combineNonInputsWithDefaults,
    verifyAssignment,
    verificationWitness,
    verificationWitnessZk,
    gateToQAP,
    gateToGenQAP,
    qapSetToMap,
    initialQapSet,
    generateAssignmentGate,
    generateAssignment,
    addMissingZeroes,
    arithCircuitToGenQAP,
    arithCircuitToQAP,
    createPolynomials,
  )
where

import Data.Euclidean (Euclidean (..))
import Data.Field (Field)
import Data.Field.Galois (GaloisField)
import Data.Foldable (foldr1)
import Data.Map (fromList, mapKeys)
import qualified Data.Map as Map
import qualified Data.Map.Merge.Lazy as Merge
import Data.Pairing.BLS12381 (Fr, getRootOfUnity)
import Data.Poly (VPoly, monomial)
import Juvix.Backends.Plonk (ArithCircuit (..), Gate (..), Wire (..), affineCircuitToAffineMap, evalArithCircuit, evalGate)
import Juvix.Library hiding (Field, quotRem)
import qualified Ref.Groth.FFT as FFT
import Text.PrettyPrint.Leijen.Text
  ( Pretty (..),
    enclose,
    indent,
    lbracket,
    rbracket,
    text,
    vcat,
    (<+>),
  )

-- | The sets of polynomials/constants as they occur in QAPs, grouped
-- into their constant, input, output and intermediate parts.
data QapSet f = QapSet
  { qapSetConstant :: f,
    qapSetInput :: Map Int f,
    qapSetIntermediate :: Map Int f,
    qapSetOutput :: Map Int f
  }
  deriving (Show, Eq, Functor, Foldable, Generic, NFData)

-- | Quadratic arithmetic program
data QAP f = QAP
  { qapInputsLeft :: QapSet (VPoly f),
    qapInputsRight :: QapSet (VPoly f),
    qapOutputs :: QapSet (VPoly f),
    qapTarget :: VPoly f
  }
  deriving (Show, Eq, Generic, NFData)

-- | Generalised quadratic arithmetic program: instead of @Poly@, allow
-- any functor.
data GenQAP p f = GenQAP
  { genQapInputsLeft :: QapSet (p f),
    genQapInputsRight :: QapSet (p f),
    genQapOutputs :: QapSet (p f),
    genQapTarget :: p f
  }
  deriving (Show, Eq, Generic, NFData)

-- Note that we could get "sequence" from the Traversable instance of
-- lists if we had an Applicative/Monad instance of QapSet. There do
-- not seem to be sensible instances of those classes for QapSet.
sequenceQapSet :: [QapSet f] -> QapSet [f]
sequenceQapSet qapSets = QapSet constants inputs mids outputs
  where
    constants = map qapSetConstant qapSets
    inputs = Map.unionsWith (<>) . fmap (fmap pure) $ map qapSetInput qapSets
    mids = Map.unionsWith (<>) . fmap (fmap pure) $ map qapSetIntermediate qapSets
    outputs = Map.unionsWith (<>) . fmap (fmap pure) $ map qapSetOutput qapSets

-- | Create QapSet with only a constant value and empty maps for the
-- rest.
constantQapSet :: g -> QapSet g
constantQapSet g =
  QapSet
    { qapSetConstant = g,
      qapSetInput = Map.empty,
      qapSetIntermediate = Map.empty,
      qapSetOutput = Map.empty
    }

cnstInpQapSet :: g -> Map Wire g -> QapSet g
cnstInpQapSet g assignment =
  QapSet
    { qapSetConstant = g,
      qapSetInput = inputs,
      qapSetIntermediate = intermediates,
      qapSetOutput = outputs
    }
  where
    inputs =
      Map.foldlWithKey'
        ( \acc k v -> case k of
            InputWire i -> Map.insert i v acc
            _ -> acc
        )
        Map.empty
        assignment
    intermediates =
      Map.foldlWithKey'
        ( \acc k v -> case k of
            IntermediateWire i -> Map.insert i v acc
            _ -> acc
        )
        Map.empty
        assignment
    outputs =
      Map.foldlWithKey'
        ( \acc k v -> case k of
            OutputWire i -> Map.insert i v acc
            _ -> acc
        )
        Map.empty
        assignment

-- | Sum all the values contained in a QapSet.
sumQapSet :: Monoid g => QapSet g -> g
sumQapSet = fold

-- | Sum only over constant and input values
sumQapSetCnstInp :: Monoid g => QapSet g -> g
sumQapSetCnstInp (QapSet cnst inp _ _) =
  cnst <> fold inp

-- | Sum only over intermediate and output values
sumQapSetMidOut :: Monoid g => QapSet g -> g
sumQapSetMidOut (QapSet _ _ mid out) =
  fold mid <> fold out

instance Pretty (Ratio Integer) where
  pretty = text . show

instance Pretty f => Pretty (QapSet f) where
  pretty (QapSet constant inps mids outps) =
    vcat
      [ text "constant:" <+> pretty constant,
        text "inputs:",
        indent 2 $ ppMap inps,
        text "outputs:",
        indent 2 $ ppMap outps,
        text "intermediates:",
        indent 2 $ ppMap mids
      ]
    where
      ppMap =
        vcat
          . map (\(ix, x) -> enclose lbracket rbracket (pretty ix) <+> pretty x)
          . Map.toList

combineWithDefaults ::
  -- | function to combine the values with
  (a -> b -> c) ->
  -- | default left value
  a ->
  -- | default right value
  b ->
  -- | left QapSet
  QapSet a ->
  -- | right QapSet
  QapSet b ->
  QapSet c
combineWithDefaults f defaultA defaultB (QapSet cA inpA midA outpA) (QapSet cB inpB midB outpB) =
  QapSet
    { qapSetConstant = f cA cB,
      qapSetInput = combineMaps inpA inpB,
      qapSetIntermediate = combineMaps midA midB,
      qapSetOutput = combineMaps outpA outpB
    }
  where
    combineMaps = Merge.merge missingRight missingLeft matching
    missingLeft = Merge.mapMissing $ const $ f defaultA
    missingRight = Merge.mapMissing $ const $ flip f defaultB
    matching = Merge.zipWithMatched $ const f

combineInputsWithDefaults ::
  -- | function to combine the values with
  (a -> b -> c) ->
  -- | default left value
  a ->
  -- | default right value
  b ->
  -- | left QapSet
  QapSet a ->
  -- | right QapSet
  QapSet b ->
  QapSet c
combineInputsWithDefaults f defaultA defaultB (QapSet cA inpA _ _) (QapSet cB inpB _ _) =
  QapSet
    { qapSetConstant = f cA cB,
      qapSetInput = combineMaps inpA inpB,
      qapSetIntermediate = mempty,
      qapSetOutput = mempty
    }
  where
    combineMaps = Merge.merge missingRight missingLeft matching
    missingLeft = Merge.mapMissing $ const $ f defaultA
    missingRight = Merge.mapMissing $ const $ flip f defaultB
    matching = Merge.zipWithMatched $ const f

combineNonInputsWithDefaults ::
  -- | function to combine the values with
  (a -> b -> c) ->
  -- | default left value
  a ->
  -- | default right value
  b ->
  -- | default constant
  c ->
  -- | left QapSet
  QapSet a ->
  -- | right QapSet
  QapSet b ->
  QapSet c
combineNonInputsWithDefaults f defaultA defaultB defaultC (QapSet _ _ midA outpA) (QapSet _ _ midB outpB) =
  QapSet
    { qapSetConstant = defaultC,
      qapSetInput = mempty,
      qapSetIntermediate = combineMaps midA midB,
      qapSetOutput = combineMaps outpA outpB
    }
  where
    combineMaps = Merge.merge missingRight missingLeft matching
    missingLeft = Merge.mapMissing $ const $ f defaultA
    missingRight = Merge.mapMissing $ const $ flip f defaultB
    matching = Merge.zipWithMatched $ const f

-- | Fold over a QapSet with an operation that is assumed to be
-- commutative.
foldQapSet ::
  -- | *commutative* binary operation
  (a -> a -> a) ->
  -- | QapSet to fold over
  QapSet a ->
  a
foldQapSet = foldr1

-- Alternative to @sequenceGenQap@
createMapGenQap :: GaloisField k => [GenQAP ((,) k) k] -> GenQAP (Map k) k
createMapGenQap genQaps = GenQAP inpLefts inpRights outputs targets
  where
    inpLefts = fmap Map.fromList . sequenceQapSet . map genQapInputsLeft $ genQaps
    inpRights = fmap Map.fromList . sequenceQapSet . map genQapInputsRight $ genQaps
    outputs = fmap Map.fromList . sequenceQapSet . map genQapOutputs $ genQaps
    targets = Map.fromList . map genQapTarget $ genQaps

instance (Eq f, Num f, Pretty f, Show f) => Pretty (QAP f) where
  pretty (QAP inpsLeft inpsRight outps target) =
    vcat
      [ text "QAP:",
        text "inputs left:",
        indent 2 . text . show $ inpsLeft,
        text "inputs right:",
        indent 2 . text . show $ inpsRight,
        text "outputs:",
        indent 2 . text . show $ outps,
        text "target: " <> text (show target)
      ]

instance (Pretty f, Pretty (p f)) => Pretty (GenQAP p f) where
  pretty (GenQAP inpsLeft inpsRight outps target) =
    vcat
      [ text "QAP:",
        text "inputs left:",
        indent 2 $ pretty inpsLeft,
        text "inputs right:",
        indent 2 $ pretty inpsRight,
        text "outputs:",
        indent 2 $ pretty outps,
        text "target: " <> pretty target
      ]

instance Functor p => Functor (GenQAP p) where
  fmap f (GenQAP inpLeft inpRight outp target) =
    GenQAP
      (fmap (fmap f) inpLeft)
      (fmap (fmap f) inpRight)
      (fmap (fmap f) outp)
      (fmap f target)

-- | Verify whether an assignment of variables is consistent with the
-- given QAP
verifyAssignment ::
  (Eq f, Field f, Num f) =>
  -- | circuit whose evaluation we want to verify
  QAP f ->
  -- | vector containing the inputs, outputs and
  -- intermediate values (outputs of all the mul-gates)
  QapSet f ->
  Bool
verifyAssignment qap assignment = isJust $ verificationWitness qap assignment

-- | Produce the polynomial witnessing the validity of given
-- assignment against the given QAP. Will return @Nothing@ if the
-- assignment is not valid.
--
-- In Pinocchio's terminology: this produces the h(x) such that p(x) =
-- h(x) * t(x) where t(x) is the target polynomial and p(x) is the
-- left input polynomials times the right input polynomials minus the
-- output polynomials.
verificationWitness ::
  forall f.
  (Eq f, Field f, Num f) =>
  -- | circuit whose evaluation we want to verify
  QAP f ->
  -- | vector containing the inputs, outputs and
  -- intermediate values (outputs of all the mul-gates)
  QapSet f ->
  Maybe (VPoly f)
verificationWitness = verificationWitnessZk 0 0 0

verificationWitnessZk ::
  (Eq f, Field f, Num f) =>
  f ->
  f ->
  f ->
  -- | circuit whose evaluation we want to verify
  QAP f ->
  -- | vector containing the inputs, outputs and
  -- intermediate values (outputs of all the mul-gates)
  QapSet f ->
  Maybe (VPoly f)
verificationWitnessZk delta1 delta2 delta3 QAP {..} assignment =
  if remainder == 0
    then Just quotient
    else Nothing
  where
    scaleWithAssignment x = combineWithDefaults (\a b -> monomial 0 b * a) 0 0 x assignment
    leftInputPoly =
      (monomial 0 delta1 * qapTarget)
        + sumQap (scaleWithAssignment qapInputsLeft)
    rightInputPoly =
      (monomial 0 delta2 * qapTarget)
        + sumQap (scaleWithAssignment qapInputsRight)
    outputPoly =
      (monomial 0 delta3 * qapTarget)
        + sumQap (scaleWithAssignment qapOutputs)
    sumQap = foldQapSet (+)
    inputOutputPoly =
      (leftInputPoly * rightInputPoly) - outputPoly
    (quotient, remainder) = quotRem inputOutputPoly qapTarget

-- | Lookup the value at the given wire label in the
-- @QapSet@.
lookupAtWire :: Wire -> QapSet a -> Maybe a
lookupAtWire (InputWire ix) QapSet {qapSetInput = inps} =
  Map.lookup ix inps
lookupAtWire (IntermediateWire ix) QapSet {qapSetIntermediate = mids} =
  Map.lookup ix mids
lookupAtWire (OutputWire ix) QapSet {qapSetOutput = outps} =
  Map.lookup ix outps

-- | Update the value at the given wire label in the
-- @QapSet@. (Partial function at the moment.)
updateAtWire :: Wire -> a -> QapSet a -> QapSet a
updateAtWire (InputWire ix) a qs@QapSet {qapSetInput = inps} =
  qs {qapSetInput = Map.insert ix a inps}
updateAtWire (IntermediateWire ix) a qs@QapSet {qapSetIntermediate = mids} =
  qs {qapSetIntermediate = Map.insert ix a mids}
updateAtWire (OutputWire ix) a qs@QapSet {qapSetOutput = outps} =
  qs {qapSetOutput = Map.insert ix a outps}

-- | Update at multiple wires
updateAtWires :: [(Wire, a)] -> QapSet a -> QapSet a
updateAtWires wireVals vars =
  foldl' (\rest (wire, val) -> updateAtWire wire val rest) vars wireVals

-- | Convert a single multiplication- or equality-gate into a QAP
gateToQAP ::
  GaloisField k =>
  (Int -> k) ->
  -- | arbitrarily chosen roots
  [k] ->
  -- | circuit to encode as a QAP
  Gate Wire k ->
  QAP k
gateToQAP primRoots roots =
  createPolynomials primRoots . addMissingZeroes roots . createMapGenQap . gateToGenQAP roots

-- | Convert a single multiplication gate (with affine circuits for
-- inputs) into a GenQAP
gateToGenQAP ::
  GaloisField k =>
  -- | arbitrarily chosen roots
  [k] ->
  -- | circuit to encode as a QAP
  Gate Wire k ->
  [GenQAP ((,) k) k]
gateToGenQAP [root] (MulGate l r wire) =
  pure
    . addOutputVals
    . addInputVals
    $ GenQAP
      { genQapInputsLeft = constantQapSet (root, leftInputConst),
        genQapInputsRight = constantQapSet (root, rightInputConst),
        genQapOutputs = constantQapSet (root, 0),
        genQapTarget = (root, 0)
      }
  where
    (leftInputConst, leftInputVector) = affineCircuitToAffineMap l
    (rightInputConst, rightInputVector) = affineCircuitToAffineMap r
    addInputVals (GenQAP left right out t) =
      GenQAP
        (Map.foldrWithKey updateAtWire left $ fmap (root,) leftInputVector)
        (Map.foldrWithKey updateAtWire right $ fmap (root,) rightInputVector)
        out
        t
    addOutputVals (GenQAP left right out t) =
      GenQAP
        left
        right
        (updateAtWire wire (root, 1) out)
        t
gateToGenQAP [root0, root1] (EqualGate i m outputWire) =
  [qap0, qap1]
  where
    qap0 =
      GenQAP
        { genQapInputsLeft =
            updateAtWires
              [ (i, (root0, 1)),
                (m, (root0, 0)),
                (outputWire, (root0, 0))
              ]
              $ constantQapSet (root0, 0),
          genQapInputsRight =
            updateAtWires
              [ (i, (root0, 0)),
                (m, (root0, 1)),
                (outputWire, (root0, 0))
              ]
              $ constantQapSet (root0, 0),
          genQapOutputs =
            updateAtWires
              [ (i, (root0, 0)),
                (m, (root0, 0)),
                (outputWire, (root0, 1))
              ]
              $ constantQapSet (root0, 0),
          genQapTarget =
            (root0, 0)
        }
    qap1 =
      GenQAP
        { genQapInputsLeft =
            updateAtWires
              [ (i, (root1, 0)),
                (m, (root1, 0)),
                (outputWire, (root1, -1))
              ]
              $ constantQapSet (root1, 1),
          genQapInputsRight =
            updateAtWires
              [ (i, (root1, 1)),
                (m, (root1, 0)),
                (outputWire, (root1, 0))
              ]
              $ constantQapSet (root1, 0),
          genQapOutputs =
            updateAtWires
              [ (i, (root1, 0)),
                (m, (root1, 0)),
                (outputWire, (root1, 0))
              ]
              $ constantQapSet (root1, 0),
          genQapTarget =
            (root1, 0)
        }
-- gateToGenQAP (root : roots) (Split inp outputs) =
--   if length roots /= length outputs
--     then panic "gateToGenQAP: wrong number of roots supplied"
--     else qap0 : zipWith qaps roots outputs
--   where
--     qap0 =
--       GenQAP
--         { genQapInputsLeft =
--             updateAtWires ((inp, (root, 0)) : zipWith (\output i -> (output, (root, pow 2 i))) outputs [0 :: Integer ..]) $
--               constantQapSet (root, 0),
--           genQapInputsRight =
--             updateAtWires [(inp, (root, 0))] $
--               constantQapSet (root, 1),
--           genQapOutputs =
--             updateAtWires [(inp, (root, 1))] $
--               constantQapSet (root, 0),
--           genQapTarget =
--             (root, 0)
--         }
--     qaps r outp =
--       GenQAP
--         { genQapInputsLeft =
--             updateAtWire outp (r, 1) $
--               constantQapSet (r, 0),
--           genQapInputsRight =
--             updateAtWire outp (r, -1) $
--               constantQapSet (r, 1),
--           genQapOutputs =
--             updateAtWire outp (r, 0) $
--               constantQapSet (r, 0),
--           genQapTarget =
--             (r, 0)
--         }
gateToGenQAP _ _ = panic "gateToGenQAP: wrong number of roots supplied"

-- | For the left input/right input/output polynomials: turn list of
-- coordinates into a polynomial that interpolates the
-- coordinates. For the target polynomial: define it as the product of
-- all monics t_g(x) := x - r_g where r_g is the root corresponding to
-- the gate g.

-- | Create polynomials using FFT-based polynomial operations instead
-- of naive.
createPolynomials ::
  GaloisField k =>
  -- | function that gives us the primitive 2^k-th root
  -- of unity
  (Int -> k) ->
  -- | GenQAP containing the coordinates we want
  -- to interpolate
  GenQAP (Map k) k ->
  QAP k
createPolynomials primRoots (GenQAP inpLeft inpRight outp targetRoots) =
  QAP
    { qapInputsLeft = fmap (FFT.interpolate primRoots . Map.elems) inpLeft,
      qapInputsRight = fmap (FFT.interpolate primRoots . Map.elems) inpRight,
      qapOutputs = fmap (FFT.interpolate primRoots . Map.elems) outp,
      qapTarget = FFT.fftTargetPoly primRoots (Map.size targetRoots)
    }

-- | Convert an arithmetic circuit into a GenQAP: perform every step
-- of the QAP translation except the final interpolation step.
arithCircuitToGenQAP ::
  GaloisField k =>
  -- | arbitrarily chosen roots, one for each gate
  [[k]] ->
  -- | circuit to encode as a QAP
  ArithCircuit k ->
  GenQAP (Map k) k
arithCircuitToGenQAP rootsPerGate (ArithCircuit gates) =
  addMissingZeroes (concat rootsPerGate)
    . createMapGenQap
    . concat
    $ zipWith gateToGenQAP rootsPerGate gates

-- | Convert an arithmetic circuit into a QAP
arithCircuitToQAP ::
  -- | arbitrarily chosen roots, one for each gate
  [[Fr]] ->
  -- | circuit to encode as a QAP
  ArithCircuit Fr ->
  QAP Fr
arithCircuitToQAP roots circuit =
  createPolynomials getRootOfUnity $
    arithCircuitToGenQAP roots circuit

-- | Add zeroes for those roots that are missing, to prevent the
-- values in the GenQAP to be too sparse. (We can be sparse in wire
-- values, but not in values at roots, otherwise the interpolation
-- step is incorrect.)
addMissingZeroes ::
  forall f.
  (Ord f, Num f) =>
  [f] ->
  GenQAP (Map f) f ->
  GenQAP (Map f) f
addMissingZeroes allRoots (GenQAP inpLeft inpRight outp t) =
  GenQAP
    (fmap (`Map.union` allZeroes) inpLeft)
    (fmap (`Map.union` allZeroes) inpRight)
    (fmap (`Map.union` allZeroes) outp)
    (t `Map.union` allZeroes)
  where
    allZeroes :: Map f f
    allZeroes = Map.fromList . map (,0) $ allRoots

-- | Generate a valid assignment for a single gate.
generateAssignmentGate ::
  (Bits f, Fractional f) =>
  -- | program
  Gate Wire f ->
  -- | assignment
  Map Wire f ->
  QapSet f
generateAssignmentGate program assignment =
  evalGate
    lookupAtWire
    updateAtWire
    (initialQapSet assignment)
    program

-- TODO: Allow outputs
initialQapSet ::
  Num f =>
  -- | inputs
  Map Wire f ->
  QapSet f
initialQapSet assignment = QapSet 1 inputs intermediates outputs
  where
    inputs =
      Map.foldlWithKey'
        ( \acc k v -> case k of
            InputWire i -> Map.insert i v acc
            _ -> acc
        )
        Map.empty
        assignment
    intermediates =
      Map.foldlWithKey'
        ( \acc k v -> case k of
            IntermediateWire i -> Map.insert i v acc
            _ -> acc
        )
        Map.empty
        assignment
    outputs =
      Map.foldlWithKey'
        ( \acc k v -> case k of
            OutputWire i -> Map.insert i v acc
            _ -> acc
        )
        Map.empty
        assignment

generateAssignment ::
  forall f.
  (Bits f, Fractional f) =>
  -- | program
  ArithCircuit f ->
  -- | inputs
  Map Wire f ->
  QapSet f
generateAssignment circuit inputs =
  evalArithCircuit lookupAtWire updateAtWire circuit $ initialQapSet inputs

qapSetToMap :: QapSet g -> Map Int g
qapSetToMap QapSet {..} =
  fromList [(0, qapSetConstant)]
    <> mapKeys ((+) 1) qapSetInput
    <> mapKeys ((+) (1 + numOfInputs)) qapSetIntermediate
    <> mapKeys ((+) (1 + numOfInputs + numOfInterms)) qapSetOutput
  where
    numOfInputs = maxKey qapSetInput
    numOfInterms = maxKey qapSetIntermediate
    maxKey :: Map Int a -> Int
    maxKey = maximumSafe . Map.keys
    maximumSafe :: (Num a, Ord a) => [a] -> a
    maximumSafe [] = 0
    maximumSafe ls = maximum ls + 1
