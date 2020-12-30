-- |
-- - Types used internally by the Michelson backend.
module Juvix.Backends.Michelson.Compilation.Types
  ( module Juvix.Backends.Michelson.Compilation.Types,
    CoreErased.AnnTerm (..),
  )
where

import qualified Juvix.Core.Application as App
import qualified Juvix.Core.ErasedAnn.Types as CoreErased
import qualified Juvix.Core.Parameterisation as P
import Juvix.Library hiding (Type)
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.Usage as Usage
import qualified Michelson.TypeCheck as M
import qualified Michelson.Typed as MT
import qualified Michelson.Untyped as M
import qualified Michelson.Untyped.Instr as Instr

data PrimTy
  = PrimTy M.Type
  | -- extra types that need arguments
    Pair
  | Lambda
  | Map
  | BigMap
  | Option
  | List
  | Set
  | Application PrimTy (NonEmpty PrimTy)
  deriving (Show, Eq, Generic, Data)

data RawPrimVal
  = Constant (M.Value' Op)
  | Inst (Instr.InstrAbstract Op)
  | AddN
  | AddI
  | AddTimeStamp
  | AddMutez
  | NegN
  | NegI
  | SubN
  | SubI
  | SubMutez
  | SubTimeStamp
  | MulI
  | MulN
  | MulMutez
  | EDivI
  | EDivN
  | EDivMutez
  | OrB
  | ORI
  | AndI
  | AndB
  | XorI
  | XorB
  | NotI
  | NotB
  | CompareI
  | CompareS
  | CompareP
  | CompareTimeStamp
  | CompareMutez
  | CompareBytes
  | CompareHash
  | SizeMap
  | SizeSet
  | SizeList
  | SizeBytes
  | SizeS
  | MemSet
  | MemMap
  | UpdateSet
  | UpdateMap
  | UpdateBMap
  | GetMap
  | GetBMap
  | -- Extra values which need types
    Right'
  | Left'
  | Nil
  | EmptyS
  | EmptyM
  | EmptyBM
  | Cast
  | Contract
  | CreateContract
  | Loop
  | Iter
  | MapOp
  deriving (Show, Eq, Generic, Data)

type NewPrim = RawPrimVal

{-# DEPRECATED NewPrim "use RawPrimVal" #-}

type Return = App.Return (P.PrimType PrimTy) RawPrimVal

type Take = App.Take (P.PrimType PrimTy) RawPrimVal

type PrimVal = Return

toTake1 :: PrimVal -> Maybe Take
toTake1 App.Cont {} = Nothing
toTake1 App.Return {retType, retTerm} = Just fun
  where
    fun = App.Take {usage = Usage.Omega, type' = retType, term = retTerm}

toTakes :: PrimVal -> (Take, [Take], Natural)
toTakes App.Cont {fun, args, numLeft} = (fun, args, numLeft)
toTakes App.Return {retType, retTerm} = (fun, [], 0)
  where
    fun = App.Take {usage = Usage.Omega, type' = retType, term = retTerm}

fromReturn :: Return -> PrimVal
fromReturn = identity

type RawTerm = CoreErased.AnnTerm PrimTy RawPrimVal

type Term = CoreErased.AnnTerm PrimTy PrimVal

type NewTerm = RawTerm

{-# DEPRECATED NewTerm "use RawTerm" #-}

type Type = CoreErased.Type PrimTy

type Value = M.Value' M.ExpandedOp

type Op = M.ExpandedOp

data CompilationError
  = NotYetImplemented Text
  | InvalidInputType
  | InternalFault Text
  | DidNotTypecheck Instr.ExpandedOp M.TCError
  | DidNotTypecheckAfterOptimisation Instr.ExpandedOp M.TCError
  | NotEnoughArguments
  | NotInStack NameSymbol.T
  | -- Should never happen!
    NotEnoughStackSpace
  | OpInMichelsonValue
  | AppliedConstantToArgument
  | TooManyArguments
  deriving (Show, Eq, Generic)

-- compToPipeLineErr

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
