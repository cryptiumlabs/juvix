module Juvix.Backends.LLVM.Net.API where

-- TODO ∷ abstract all all imports to LLVM
import qualified Juvix.Backends.LLVM.Codegen as Codegen
import qualified Juvix.Backends.LLVM.DSL as DSL
import qualified Juvix.Backends.LLVM.Net.EAC.Defs as Defs
import qualified Juvix.Backends.LLVM.Net.EAC.Types as Types
import Juvix.Library hiding (reduce)
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.IntegerPredicate as IntPred
import qualified LLVM.AST.Name as Name
import qualified LLVM.AST.Operand as Operand
import qualified LLVM.AST.Type as Type

defineCreateNet ∷ Codegen.Define m ⇒ m Operand.Operand
defineCreateNet =
  Codegen.defineFunction Type.void "createNet" [] $
    -- TODO
    pure ()

defineReadNet ∷ Codegen.Define m ⇒ m Operand.Operand
defineReadNet =
  Codegen.defineFunction Type.void "readNet" [] $
    -- TODO
    pure ()

defineSaveState ∷ Codegen.Define m ⇒ m Operand.Operand
defineSaveState =
  Codegen.defineFunction Type.void "saveState" [] $
    -- TODO
    pure ()

defineLoadState ∷ Codegen.Define m ⇒ m Operand.Operand
defineLoadState =
  Codegen.defineFunction Type.void "loadState" [] $
    -- TODO
    pure ()

defineAppendToNet ∷ Codegen.Define m ⇒ m Operand.Operand
defineAppendToNet =
  Codegen.defineFunction Type.void "appendToNet" [] $
    -- TODO
    pure ()

defineReduceUntilComplete ∷ Codegen.Define m ⇒ m Operand.Operand
defineReduceUntilComplete =
  Codegen.defineFunction Type.void "reduceUntilComplete" [] $
    -- TODO
    pure ()
