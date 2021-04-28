{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}

module Juvix.Backends.Plonk.Circuit.Assignment 
( Assignment(..)
, generateAssignment
, updateAtWire
, lookupAtWire
, initialAssignment
)
where

import qualified Data.Map as Map
import Juvix.Backends.Plonk.Circuit
import Juvix.Library

-- | The sets of polynomials/constants as they occur in QAPs, grouped
-- into their constant, input, output and intermediate parts.
data Assignment f = Assignment
  { assignmentConstant :: f,
    assignmentInput :: Map Int f,
    assignmentIntermediate :: Map Int f,
    assignmentOutput :: Map Int f
  }
  deriving (Show, Eq, Functor, Foldable, Generic)

generateAssignment ::
  forall f.
  (Bits f, Fractional f) =>
  -- | program
  ArithCircuit f ->
  -- | inputs
  Map Wire f ->
  Assignment f
generateAssignment circuit inputs =
  evalArithCircuit lookupAtWire updateAtWire circuit $ initialAssignment inputs

-- | Update the value at the given wire label in the
-- @Assignment@. (Partial function at the moment.)
updateAtWire :: Wire -> a -> Assignment a -> Assignment a
updateAtWire (InputWire ix) a qs@Assignment {assignmentInput = inps} =
  qs {assignmentInput = Map.insert ix a inps}
updateAtWire (IntermediateWire ix) a qs@Assignment {assignmentIntermediate = mids} =
  qs {assignmentIntermediate = Map.insert ix a mids}
updateAtWire (OutputWire ix) a qs@Assignment {assignmentOutput = outps} =
  qs {assignmentOutput = Map.insert ix a outps}

-- | Lookup the value at the given wire label in the
-- @Assignment@.
lookupAtWire :: Wire -> Assignment a -> Maybe a
lookupAtWire (InputWire ix) Assignment {assignmentInput = inps} =
  Map.lookup ix inps
lookupAtWire (IntermediateWire ix) Assignment {assignmentIntermediate = mids} =
  Map.lookup ix mids
lookupAtWire (OutputWire ix) Assignment {assignmentOutput = outps} =
  Map.lookup ix outps

initialAssignment ::
  Num f =>
  -- | inputs
  Map Wire f ->
  Assignment f
initialAssignment assignment = Assignment 1 inputs mempty mempty
  where
    inputs =
      Map.foldlWithKey'
        ( \acc k v -> case k of
            InputWire i -> Map.insert i v acc
            _ -> acc
        )
        Map.empty
        assignment
