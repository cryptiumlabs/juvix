-- |
-- - Types used internally by the Michelson backend.
module Juvix.Backends.Michelson.Compilation.Types where

import qualified Juvix.Core.ErasedAnn.Types as CoreErased
import Juvix.Library
import qualified Michelson.TypeCheck as M
import qualified Michelson.Typed as MT
import qualified Michelson.Untyped as M

data PrimTy
  = PrimTy M.Type
  deriving (Show, Eq, Generic)

data PrimVal
  = PrimConst (M.Value' Op)
  | PrimPair
  | PrimFst
  | PrimSnd
  -- TODO: Add all Michelson instructions which are functions.
  deriving (Show, Eq, Generic)

type Term = CoreErased.AnnTerm PrimTy PrimVal

type Type = CoreErased.Type PrimTy PrimVal

type Value = M.Value' M.ExpandedOp

type Op = M.ExpandedOp

data CompilationError
  = NotYetImplemented Text
  | InvalidInputType
  | InternalFault Text
  | DidNotTypecheck M.TCError
  | DidNotTypecheckAfterOptimisation M.TCError
  | -- Should never happen!
    NotEnoughStackSpace
  deriving (Show, Eq, Generic)

data CompilationLog
  = TermToInstr Term Op
  | OptimisedByJuvix Op Op
  | OptimisedByMorley SomeInstr SomeInstr
  deriving (Generic)

data SomeInstr where
  SomeInstr ∷ ∀ a b. MT.Instr a b → SomeInstr

deriving instance Show (SomeInstr)

instance Eq SomeInstr where
  _ == _ = False
