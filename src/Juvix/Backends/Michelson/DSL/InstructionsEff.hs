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

import qualified Data.Set as Set
import qualified Juvix.Backends.Michelson.Compilation.Types as Types
import qualified Juvix.Backends.Michelson.Compilation.VirtualStack as VStack
import qualified Juvix.Backends.Michelson.DSL.Environment as Env
import qualified Juvix.Backends.Michelson.DSL.Instructions as Instructions
import Juvix.Library
import qualified Michelson.Untyped.Instr as Instr
import qualified Michelson.Untyped.Value as V
import Prelude (error)

--------------------------------------------------------------------------------
-- Main Functionality
--------------------------------------------------------------------------------

inst ∷ Env.Reduction m ⇒ Types.NewTerm → m Env.Expanded
inst = undefined

addAll ∷ Env.Reduction m ⇒ Types.NewTerm → Types.NewTerm → m Env.Expanded
addAll instr1 instr2 = add [instr2, instr1]

add, mul, sub, ediv, and, xor, or ∷ Env.Reduction m ⇒ [Types.NewTerm] → m Env.Expanded
add = intGen Instructions.add (+)
mul = intGen Instructions.mul (*)
sub = intGen Instructions.sub (-)
and = onBoolGen Instructions.and (&&)
xor = onBoolGen Instructions.xor (/=)
or = onBoolGen Instructions.or (||)
ediv =
  onIntGen
    Instructions.ediv
    ( \x y → case y of
        0 → V.ValueNone
        y → V.ValueSome (V.ValuePair (V.ValueInt (x `div` y)) (V.ValueInt (rem x y)))
    )

-- naive dup to front logic

var ∷ (Env.Instruction m, Env.Error m) ⇒ Symbol → m Env.Expanded
var symb = do
  stack ← get @"stack"
  case VStack.lookup symb stack of
    Nothing →
      throw @"compilationError" (Types.NotInStack symb)
    Just (VStack.Value (VStack.Val' value)) →
      pure (Env.Constant value)
    Just (VStack.Value (VStack.Lam' lamPartial)) →
      pure (Env.Curr lamPartial)
    Just (VStack.Position usage index)
      | one == usage →
        Env.Expanded <$> moveToFront index
      | otherwise →
        Env.Expanded <$> dupToFront index

--------------------------------------------------------------------------------
-- Reduction Helpers for Main functionality
--------------------------------------------------------------------------------

type OnTerm m input result =
  Env.Reduction m ⇒
  Instr.ExpandedOp →
  (input → input → result) →
  [Types.NewTerm] →
  m Env.Expanded

onBoolGen ∷ OnTerm m Bool Bool
onBoolGen op f =
  onTwoArgs
    op
    ( \instr1 instr2 →
        let i1 = valToBool instr1
            i2 = valToBool instr2
         in Env.Constant (boolToVal (f i1 i2))
    )

intGen ∷ OnTerm m Integer Integer
intGen op f = onIntGen op (\x y → V.ValueInt (f x y))

onIntGen ∷ OnTerm m Integer (V.Value' Types.Op)
onIntGen op f =
  onTwoArgs
    op
    ( \instr1 instr2 →
        let V.ValueInt i1 = instr1
            V.ValueInt i2 = instr2
         in Env.Constant (f i1 i2)
    )

onTwoArgs ∷ OnTerm m (V.Value' Types.Op) Env.Expanded
onTwoArgs op f instrs = do
  v ← traverse (protect . inst) instrs
  case v of
    instr2 : instr1 : _ →
      let instrs = [instr2, instr1]
       in if
            | allConstants (val <$> instrs) →
              let Env.Constant i1 = val instr1
                  Env.Constant i2 = val instr2
               in pure (f i1 i2)
            | otherwise → do
              traverse_ addExpanded instrs
              addInstr op
              pure (Env.Expanded op)
    _ → throw @"compilationError" Types.NotEnoughArguments

-------------------------------------------------------------------------------
-- Environment Protections, Promotions, and Movements
--------------------------------------------------------------------------------

moveToFront ∷ (Env.Instruction m, Integral a) ⇒ a → m Instr.ExpandedOp
moveToFront num = do
  let inst = Instructions.dig (fromIntegral num)
  addInstr inst
  modify @"stack" (VStack.dig (fromIntegral num))
  pure inst

dupToFront ∷ (Env.Instruction m, Integral a) ⇒ a → m Instr.ExpandedOp
dupToFront num = do
  modify @"stack" (VStack.dupDig (fromIntegral num))
  let instrs =
        [ Instructions.dig (fromIntegral num),
          Instructions.dup,
          Instructions.dug (fromIntegral num)
        ]
  addInstrs instrs
  pure (fold instrs)

data Protect
  = Protect
      { val ∷ Env.Expanded,
        insts ∷ [Types.Op]
      }

protect ∷ Env.Ops m ⇒ m Env.Expanded → m Protect
protect inst = do
  curr ← get @"ops"
  v ← inst
  after ← get @"ops"
  put @"ops" curr
  pure Protect {val = v, insts = after}

-- for constant intructions the list should be empty!
-- This should actually promote lambda and curry into a Lambda Michelson form
-- This is because when this is called it'll only be called in a function like
-- map and fold
addExpanded ∷ Env.Ops m ⇒ Protect → m ()
addExpanded (Protect (Env.Expanded _) i) = addInstrs i
addExpanded (Protect (Env.Constant v) _) = addInstr (Instructions.push undefined v)
addExpanded (Protect (Env.Curr {}) _) = undefined

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

--------------------------------------------------------------------------------
-- Misc
--------------------------------------------------------------------------------

allConstants ∷ [Env.Expanded] → Bool
allConstants = all f
  where
    f (Env.Constant _) = True
    f (Env.Expanded _) = False
    f (Env.Curr {}) = True
