-- |
-- Serves as a module for various debugging functions
module Juvix.Backends.LLVM.Net.EAC.Debug where

import qualified Juvix.Backends.LLVM.Codegen as Codegen
import qualified LLVM.AST.Operand as Operand

printNodePort ::
  ( Codegen.Call m,
    Codegen.NewBlock m
  ) =>
  Operand.Operand ->
  Operand.Operand ->
  m ()
printNodePort = Codegen.printNodePort
