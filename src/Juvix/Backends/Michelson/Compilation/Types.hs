-- |
-- - Types used internally by the Michelson backend.
module Juvix.Backends.Michelson.Compilation.Types
  ( module Juvix.Backends.Michelson.Compilation.Types,
    CoreErased.AnnTerm (..),
  )
where

import qualified Juvix.Core.ErasedAnn.Types as CoreErased
import Juvix.Library
import qualified Michelson.TypeCheck as M
import qualified Michelson.Typed as MT
import qualified Michelson.Untyped as M
import qualified Michelson.Untyped.Instr as Instr

newtype PrimTy
  = PrimTy M.Type
  deriving (Show, Eq, Generic)

data NewPrim
  = Constant (M.Value' Op)
  | Inst (Instr.InstrAbstract Op)
  deriving (Show, Eq, Generic)

type NewTerm = CoreErased.AnnTerm PrimTy NewPrim

type PrimVal = NewPrim

type Term = CoreErased.AnnTerm PrimTy NewPrim

type Type = CoreErased.Type PrimTy NewPrim

type Value = M.Value' M.ExpandedOp

type Op = M.ExpandedOp

data CompilationError
  = NotYetImplemented Text
  | InvalidInputType
  | InternalFault Text
  | DidNotTypecheck Instr.ExpandedOp M.TCError
  | DidNotTypecheckAfterOptimisation Instr.ExpandedOp M.TCError
  | NotEnoughArguments
  | NotInStack Symbol
  | -- Should never happen!
    NotEnoughStackSpace
  deriving (Show, Eq, Generic)

data CompilationLog
  = TermToInstr Term Op
  | OptimisedByJuvix Op Op
  | OptimisedByMorley SomeInstr SomeInstr
  deriving (Generic, Show)

data EmptyInstr where
  EmptyInstr :: forall b. MT.Instr '[] b -> EmptyInstr

data SomeInstr where
  SomeInstr :: forall a b. MT.Instr a b -> SomeInstr

deriving instance Show SomeInstr

deriving instance Show EmptyInstr

instance Eq SomeInstr where
  _ == _ = False
