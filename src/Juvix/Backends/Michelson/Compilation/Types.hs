module Juvix.Backends.Michelson.Compilation.Types where

import Juvix.Backends.Michelson.Parameterisation
import Juvix.Library
import qualified Michelson.TypeCheck as M
import qualified Michelson.Typed as MT
import qualified Michelson.Untyped as M

data CompilationError
  = NotYetImplemented Text
  | InvalidInputType
  | InternalFault Text
  | DidNotTypecheck M.TCError
  deriving (Show, Eq, Generic)

data CompilationLog
  = TermToInstr Term Op
  | Optimised SomeInstr SomeInstr
  deriving (Generic)

type Stack = [(StackElem, M.Type)]

data StackElem
  = ConstE Value
  | VarE Symbol
  | FuncResultE
  deriving (Show, Eq, Generic)

data SomeInstr where
  SomeInstr ∷ ∀ a b. MT.Instr a b → SomeInstr
