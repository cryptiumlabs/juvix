module Juvix.Backends.ArithmeticCircuit.Compilation.Types where

import qualified Data.Graph.Inductive as G
import Juvix.Library

type Circuit = G.Gr NodeType WireType

data NodeType
  = -- Addition gate.
    Add
  | -- Multiplication gate.
    Mul
  | -- Input value (by vector index).
    InputValue Natural
  | -- Output value (by vector index).
    OutputValue Natural
  deriving (Show, Eq, Generic)

type WireType = ()
