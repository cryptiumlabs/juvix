{-# LANGUAGE ConstraintKinds #-}

module Juvix.Backends.Michelson.DSL.Environment where

import qualified Data.Set as Set
import qualified Juvix.Backends.Michelson.Compilation.Types as Types
import qualified Juvix.Backends.Michelson.Compilation.VirtualStack as VStack
import Juvix.Library hiding (show)
import qualified Michelson.Untyped.Instr as Instr
import qualified Michelson.Untyped.Value as V
import Prelude (Show (..))

data Env
  = Env
      { -- | The Virtual stack that mimics the real Michelson stack.
        stack ∷ VStack.T Curr,
        -- | Information during Compilation.
        compilationLog ∷ [Types.CompilationLog],
        -- | The Operations for the Michelson Contract.
        ops ∷ [Types.Op],
        -- | count of unnamed arguments for primitives.
        count ∷ Word,
        -- | Debug level.
        debug ∷ Int
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
  | Expanded (Instr.ExpandedOp)
  | -- | Curr is a stand in for lambda or curry
    Curr Curr
  deriving (Show)

newtype Fun = Fun (∀ m. Reduction m ⇒ [Types.NewTerm] → m Expanded)

unFun ∷ Reduction m ⇒ Fun → [Types.NewTerm] → m Expanded
unFun (Fun f) = f

data Curr
  = C
      { -- | The function itself that we will call when we have enough arguments
        --   To expand
        fun ∷ Fun,
        -- | 'argsLeft' are the arguments that are left on the stack
        argsLeft ∷ [Symbol],
        -- | 'argsApplied' are the names of the arguments that have been already applied
        argsApplied ∷ [Symbol],
        -- | 'left' are the number of arguments left.
        --   This number should be (length 'argsLeft')
        left ∷ Integer,
        -- | 'captures' are the captured arguments in the environment of the function
        captures ∷ Set.Set Symbol,
        -- | 'ty' is the type of the partial
        ty ∷ Types.Type
      }

instance Show Curr where
  show (C _ al aa l c ty) =
    "C "
      <> "{ fun, argsLeft: "
      <> show al
      <> " argsApplied: "
      <> show aa
      <> " left: "
      <> show l
      <> " captures: "
      <> show c
      <> " ty: "
      <> show ty

newtype MichelsonCompilation a
  = Compilation (ExceptT CompError (State Env) a)
  deriving (Functor, Applicative, Monad)
  deriving
    ( HasStream "compilationLog" [Types.CompilationLog],
      HasWriter "compilationLog" [Types.CompilationLog]
    )
    via WriterLog (Field "compilationLog" () (MonadState (ExceptT CompError (State Env))))
  deriving
    (HasState "stack" (VStack.T Curr))
    via Field "stack" () (MonadState (ExceptT CompError (State Env)))
  deriving
    (HasState "ops" [Types.Op])
    via Field "ops" () (MonadState (ExceptT CompError (State Env)))
  deriving
    (HasState "count" Word)
    via Field "count" () (MonadState (ExceptT CompError (State Env)))
  deriving
    (HasThrow "compilationError" CompError)
    via MonadError (ExceptT CompError (State Env))
  deriving
    (HasReader "debug" Int)
    via Field "debug" () (ReadStatePure (MonadState (ExceptT CompError (State Env))))

type Ops m = HasState "ops" [Types.Op] m

type Instruction m =
  ( HasState "stack" (VStack.T Curr) m,
    Ops m
  )

type Error = HasThrow "compilationError" CompError

type Primitive m =
  ( Instruction m,
    HasState "count" Word m
  )

type Reduction m =
  ( Primitive m,
    HasThrow "compilationError" CompError m,
    HasWriter "compilationLog" [Types.CompilationLog] m
  )
