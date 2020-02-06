-- |
-- - This module includes a higher level DSL which each instruction
--   has a stack effect
--   + This is similar to the base LLVM bindings we have.
--   + So for example, emitting an =add=, eats two items from the
--     virtual stack, and adds an =Instr.Add= instruction to the
--     sequence of instructions to execute

module Juvix.Backends.Michelson.DSL.InstructionsEff where

import qualified Juvix.Backends.Michelson.DSL.Instructions as Instructions
