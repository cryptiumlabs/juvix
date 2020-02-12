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
import Prelude (error)

--------------------------------------------------------------------------------
-- Top Level Types
--------------------------------------------------------------------------------

data Top
  = Curr
  | Completed Expanded

data Expanded
  = Constant (V.Value' Types.Op)
  | Expanded (Instr.ExpandedOp)

--------------------------------------------------------------------------------
-- Main Functionality
--------------------------------------------------------------------------------

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

add = intGen Instructions.add (+)

mul = intGen Instructions.mul (*)

sub = intGen Instructions.sub (-)

ediv =
  onIntGen
    Instructions.ediv
    ( \x y → case y of
        0 → V.ValueNone
        y → V.ValueSome (V.ValuePair (V.ValueInt (x `div` y)) (V.ValueInt (rem x y)))
    )

and = onBoolGen Instructions.and (&&)

xor = onBoolGen Instructions.xor (/=)

or = onBoolGen Instructions.or (||)

--------------------------------------------------------------------------------
-- Helpers for Main Functionality
--------------------------------------------------------------------------------

onBoolGen op f =
  onTwoArgs
    op
    ( \instr1 instr2 →
        let i1 = valToBool instr1
            i2 = valToBool instr2
         in Constant (boolToVal (f i1 i2))
    )

intGen op f = onIntGen op (\x y → V.ValueInt (f x y))

onIntGen op f =
  onTwoArgs
    op
    ( \instr1 instr2 →
        let V.ValueInt i1 = instr1
            V.ValueInt i2 = instr2
         in Constant (f i1 i2)
    )

onTwoArgs op f instrs = do
  instr2 : instr1 : _ ← traverse (protect . inst) instrs
  let instrs = [instr2, instr1]
  if
    | allConstants (val <$> instrs) →
      let Constant i1 = val instr1
          Constant i2 = val instr2
       in pure (f i1 i2)
    | otherwise → do
      traverse_ addExpanded instrs
      addInstr op
      pure (Expanded op)

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

-- for constant intructions the list should be empty!
addExpanded ∷ Env.Ops m ⇒ Protect → m ()
addExpanded (Protect (Expanded _) i) = addInstrs i
addExpanded (Protect (Constant v) _) = addInstr (Instructions.push undefined v)

--------------------------------------------------------------------------------
-- Effect Wrangling
--------------------------------------------------------------------------------

addInstrs ∷ Env.Ops m ⇒ [Instr.ExpandedOp] → m ()
addInstrs x = modify @"ops" (x <>)

addInstr ∷ Env.Ops m ⇒ Instr.ExpandedOp → m ()
addInstr x = modify @"ops" (x :)

--------------------------------------------------------------------------------
-- Boolean Conversions
--------------------------------------------------------------------------------

boolToVal ∷ Bool → V.Value' op
boolToVal True = V.ValueTrue
boolToVal False = V.ValueFalse

valToBool ∷ V.Value' op → Bool
valToBool V.ValueTrue = True
valToBool V.ValueFalse = False
valToBool _ = error "called valToBool on a non Michelson Bool"
