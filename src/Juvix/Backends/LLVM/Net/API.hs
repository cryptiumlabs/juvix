module Juvix.Backends.LLVM.Net.API where

-- TODO ∷ abstract all all imports to LLVM
import qualified Juvix.Backends.LLVM.Codegen as Codegen
-- -- import qualified Juvix.Backends.LLVM.DSL as DSL
-- -- import qualified Juvix.Backends.LLVM.Net.EAC.Defs as Defs
-- -- import qualified Juvix.Backends.LLVM.Net.EAC.Types as Types
import Juvix.Library hiding (reduce)
-- -- import qualified LLVM.AST.Constant as C
-- -- import qualified LLVM.AST.IntegerPredicate as IntPred
-- -- import qualified LLVM.AST.Name as Name
import qualified LLVM.AST.Operand as Operand
import qualified LLVM.AST.Type as Type

-- TODO: c2hs / hs2c (whatever) for conversion types?
-- for now: manually

defineCreateNet ∷ Codegen.Define m ⇒ m Operand.Operand
defineCreateNet =
  Codegen.defineFunction Type.void "createNet" [(undefined, "nodes")] $ do
    -- TODO
    -- Clear the current net
    -- Call appendToNet with nodes
    Codegen.callGen Type.void [undefined] "appendToNet"
    pure ()

defineReadNet ∷ Codegen.Define m ⇒ m Operand.Operand
defineReadNet =
  Codegen.defineFunction Type.void "readNet" [] $
    -- TODO
    -- Read the current net, return a list of nodes
    pure ()

defineSaveState ∷ Codegen.Define m ⇒ m Operand.Operand
defineSaveState =
  Codegen.defineFunction Type.void "saveState" [] $
    -- TODO
    -- Save the current net state to some opaque pointer
    pure ()

defineLoadState ∷ Codegen.Define m ⇒ m Operand.Operand
defineLoadState =
  Codegen.defineFunction Type.void "loadState" [] $
    -- TODO
    -- Load the current net state from some opaque pointer
    pure ()

defineAppendToNet ∷ Codegen.Define m ⇒ m Operand.Operand
defineAppendToNet =
  Codegen.defineFunction Type.void "appendToNet" [(undefined, "nodes")] $
    -- TODO
    -- Append nodes to the net
    -- Call `createNode` on each
    -- Append to the primary pair list as necessary
    pure ()

defineReduceUntilComplete ∷ Codegen.Define m ⇒ m Operand.Operand
defineReduceUntilComplete =
  Codegen.defineFunction Type.void "reduceUntilComplete" [] $
    -- TODO
    -- Call reduce until there are no primary pairs left
    pure ()
