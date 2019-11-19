module Juvix.Backends.LLVM.JIT.Types where

import Juvix.Library

data OptimisationLevel
  = -- TODO: Determine if none / O0 are equivalent.
    None
  | O0
  | O1
  | O2
  | O3
  deriving (Show, Eq)

data Config
  = Config
      { configOptimisationLevel ∷ OptimisationLevel
      }
  deriving (Show, Eq)
