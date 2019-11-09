module Juvix.Backends.Michelson.Compilation.Types where

import Juvix.Backends.Michelson.Parameterisation
import Juvix.Library
import Michelson.Untyped

data CompilationError
  = NotYetImplemented
  deriving (Show, Eq, Generic)

data CompilationLog
  = TermToInstr Term Instr
  deriving (Show, Eq, Generic)
