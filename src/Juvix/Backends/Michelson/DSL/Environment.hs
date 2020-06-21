{-# LANGUAGE ConstraintKinds #-}

module Juvix.Backends.Michelson.DSL.Environment where

import qualified Data.Set as Set
import qualified Juvix.Backends.Michelson.Compilation.Types as Types
import qualified Juvix.Backends.Michelson.Compilation.VirtualStack as VStack
import qualified Juvix.Core.Usage as Usage
import Juvix.Library hiding (show)
import qualified Michelson.Untyped.Instr as Instr
import qualified Michelson.Untyped.Value as V
import Prelude (Show (..))

data Env = Env
  { -- | The Virtual stack that mimics the real Michelson stack.
    stack :: VStack.T Curried,
    -- | Information during Compilation.
    compilationLog :: [Types.CompilationLog],
    -- | The Operations for the Michelson Contract.
    ops :: [Types.Op],
    -- | count of unnamed arguments for primitives.
    count :: Word,
    -- | Debug level.
    debug :: Int
  }
  deriving (Show, Generic)

type CompError = Types.CompilationError

--------------------------------------------------------------------------------
-- Top Level Types
--------------------------------------------------------------------------------

-- data Top
--   = Curr
--   | Completed Expanded

data Expanded
  = Constant (V.Value' Types.Op)
  | Expanded Instr.ExpandedOp
  | MichelsonLam
  | -- | Curr is a stand in for lambda or curry
    Curr Curried
  | -- | Nop is a non constant, that has already been added to the ops stack
    Nop
  deriving (Show)

newtype Fun = Fun (forall m. Reduction m => [Types.NewTerm] -> m Expanded)

unFun :: Reduction m => Fun -> [Types.NewTerm] -> m Expanded
unFun (Fun f) = f

data ErasedTerm = Term
  { name :: Symbol,
    usage :: Usage.T
  }
  deriving (Show)

data Curried = C
  { -- | The function itself that we will call when we have enough arguments
    --   To expand
    fun :: Fun,
    -- | 'argsLeft' are the arguments that are left on the stack
    -- This should also contain usage information!
    argsLeft :: [ErasedTerm],
    -- | 'left' are the number of arguments left.
    --   This number should be (length 'argsLeft')
    left :: Integer,
    -- | 'captures' are the captured arguments in the environment of the function
    -- This set should also contain usage information for each left
    captures :: Set.Set Symbol,
    -- | 'ty' is the type of the partial
    ty :: Types.Type
  }
  deriving (Generic)

instance Show Curried where
  show (C _ al l c ty) =
    "C "
      <> "{ fun, argsLeft: "
      <> show al
      <> " left: "
      <> show l
      <> " captures: "
      <> show c
      <> " ty: "
      <> show ty

type MichelsonCompilationAlias = ExceptT CompError (State Env)

newtype MichelsonCompilation a
  = Compilation (MichelsonCompilationAlias a)
  deriving (Functor, Applicative, Monad)
  deriving
    ( HasSink "compilationLog" [Types.CompilationLog],
      HasWriter "compilationLog" [Types.CompilationLog]
    )
    via WriterField "compilationLog" MichelsonCompilationAlias
  deriving
    ( HasState "stack" (VStack.T Curried),
      HasSink "stack" (VStack.T Curried),
      HasSource "stack" (VStack.T Curried)
    )
    via StateField "stack" MichelsonCompilationAlias
  deriving
    ( HasState "ops" [Types.Op],
      HasSink "ops" [Types.Op],
      HasSource "ops" [Types.Op]
    )
    via StateField "ops" MichelsonCompilationAlias
  deriving
    ( HasState "count" Word,
      HasSink "count" Word,
      HasSource "count" Word
    )
    via StateField "count" MichelsonCompilationAlias
  deriving
    ( HasReader "debug" Int,
      HasSource "debug" Int
    )
    via ReaderField "debug" MichelsonCompilationAlias
  deriving
    (HasThrow "compilationError" CompError)
    via MonadError MichelsonCompilationAlias

execMichelson :: MichelsonCompilation a -> (Either CompError a, Env)
execMichelson (Compilation c) = runState (runExceptT c) (Env mempty mempty mempty 0 0)

type Count m = HasState "count" Word m

type Ops m = HasState "ops" [Types.Op] m

type Stack m = HasState "stack" (VStack.T Curried) m

type Instruction m =
  ( Stack m,
    Ops m
  )

type Error = HasThrow "compilationError" CompError

type Primitive m =
  ( Instruction m,
    Count m
  )

type Reduction m =
  ( Primitive m,
    HasThrow "compilationError" CompError m,
    HasWriter "compilationLog" [Types.CompilationLog] m
  )
