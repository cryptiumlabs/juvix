module Juvix.Backends.Michelson.Compilation.Types where

import Juvix.Backends.Michelson.Parameterisation
import Juvix.Library
import qualified Michelson.Untyped as M

data CompilationError
  = NotYetImplemented
  deriving (Show, Eq, Generic)

data CompilationLog
  = TermToInstr Term M.Instr
  deriving (Show, Eq, Generic)

type Stack = [(StackElem, M.Type)]

data StackElem
  = ConstE Value
  | VarE Symbol
  | FuncResultE
  deriving (Show, Eq, Generic)
