module Juvix.Backends.Michelson.Compilation.Types where

import Juvix.Backends.Michelson.Parameterisation
import Juvix.Library
import qualified Michelson.Untyped as M
import qualified Michelson.TypeCheck as M

data CompilationError
  = NotYetImplemented
  | InvalidInputType
  | InternalFault Text
  | DidNotTypecheck M.TCError
  deriving (Show, Eq, Generic)

data CompilationLog
  = TermToInstr Term M.Instr
  | Optimised (M.SomeInstr '[]) (M.SomeInstr '[])
  deriving (Show, Generic)

type Stack = [(StackElem, M.Type)]

data StackElem
  = ConstE Value
  | VarE Symbol
  | FuncResultE
  deriving (Show, Eq, Generic)
