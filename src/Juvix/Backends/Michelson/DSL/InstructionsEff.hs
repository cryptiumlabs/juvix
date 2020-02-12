-- |
-- - This module includes a higher level DSL which each instruction
--   has a stack effect
--   + This is similar to the base LLVM bindings we have.
--   + So for example, emitting an =add=, eats two items from the
--     virtual stack, and adds an =Instr.Add= instruction to the
--     sequence of instructions to execute
-- - For constant progoation, have a function say take-2 that looks at
--   the top two items in the stack and then returns back either if
--   they were constants or not and dispatches logic based on that
module Juvix.Backends.Michelson.DSL.InstructionsEff where

import qualified Juvix.Backends.Michelson.Compilation.Types as Types
import qualified Juvix.Backends.Michelson.DSL.Environment as Env
import qualified Juvix.Backends.Michelson.DSL.Instructions as Instructions
import Juvix.Library
import qualified Juvix.Library.HashMap as Map
import qualified Michelson.Untyped.Instr as Instr
import qualified Michelson.Untyped.Value as V

data Expanded
  = Constant (V.Value' Types.Op)
  | Expanded (Instr.ExpandedOp)

envMapping =
  Map.fromList [(Instr.ADD, (+))]

-- inst ∷ Types.NewPrim → Expanded
inst = undefined

allConstants ∷ [Expanded] → Bool
allConstants = all f
  where
    f (Constant _) = True
    f (Expanded _) = False

addAll instr1 instr2 = add [instr2, instr1]

add instrs = do
  instr2 : instr1 : _ ← traverse (protect . inst) instrs
  let instrs = [instr2, instr1]
  if
    | allConstants (val <$> instrs) →
      let Constant (V.ValueInt i1) = val instr1
          Constant (V.ValueInt i2) = val instr2
       in pure (Constant (V.ValueInt (i1 + i2)))
    | otherwise → do
      traverse_ addExpanded instrs
      addInstr Instructions.add
      pure (Expanded Instructions.add)

data Protect
  = Protect
      { val ∷ Expanded,
        insts ∷ [Types.Op]
      }

protect ∷ Env.Ops m ⇒ m Expanded → m Protect
protect inst = do
  curr ← get @"ops"
  v ← inst
  after ← get @"ops"
  put @"ops" curr
  pure Protect {val = v, insts = after}

-- for the exapnded case it was already added!
addExpanded ∷ Env.Ops m ⇒ Protect → m ()
addExpanded (Protect (Expanded _) i) = addInstrs i
addExpanded (Protect (Constant v) _) = addInstr (Instructions.push undefined v)

addInstrs ∷ Env.Ops m ⇒ [Instr.ExpandedOp] → m ()
addInstrs x = modify @"ops" (x <>)

addInstr ∷ Env.Ops m ⇒ Instr.ExpandedOp → m ()
addInstr x = modify @"ops" (x :)
