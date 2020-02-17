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
import qualified Juvix.Core.ErasedAnn.Types as Ann
import qualified Juvix.Core.Usage as Usage
import Juvix.Library hiding (or, and, xor)
import qualified Michelson.Untyped.Instr as Instr
import qualified Michelson.Untyped.Type as Untyped
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

-- |
-- Name calls inst, and then determines how best to name the form in the VStack
name ∷ Env.Reduction m ⇒ Symbol → Types.NewTerm → m Env.Expanded
name symb f@(form, _usage, _type') = do
  result ← inst f
  case form of
    Ann.Var {} →
      modify @"stack" (VStack.nameTop symb)
    -- all prims shouldn't add their arguments to the vstack
    Ann.Prim {} →
      modify @"stack" (VStack.nameTop symb)
  -- consVar symb result usage type'
  pure result



-- for constant we shouldn't be applying it unless it's a lambda Ι don't think!?
primToFargs
  :: (Env.Reduction m, Num b) ⇒ Types.NewPrim → ([Types.NewTerm] → m Env.Expanded, b)
primToFargs (Types.Constant _) = undefined
primToFargs (Types.Inst inst) =
  case inst of
    Instr.ADD _ → (add, 2)
    Instr.SUB _ → (sub, 2)
    Instr.MUL _ → (mul, 2)
    Instr.EDIV _ → (ediv, 2)
    Instr.OR {} → (or, 2)
    Instr.AND _ → (and, 2)
    Instr.XOR _ → (xor, 2)


appM form@(t,u,ty) args usage type' =
  case t of
    Ann.Prim p →
      let (f,lPrim) = primToFargs p
          argsL = length args
      in case argsL `compare` lPrim of
        EQ → f args
        -- TODO ∷ bind names from env, apply them to current args
        -- take more args and stick them in the env, then have the
        -- form of the application be only locations?
        LT → undefined
        -- this should never happen, due to type checking??
        GT →
          throw
            @"compilationError"
            (Types.InternalFault "Michelson call with too many args")
    Ann.LamM {} → undefined
    Ann.Var _ → do
      v ← inst form
      case v of
        Env.Curr c → undefined
        -- these two cases would only be valid if we expanded into a Michelson
        -- lambda, however would we ever do that?
        Env.Constant _ → throw @"compilationError" (Types.InternalFault "App on Constant")
        Env.Expanded _ → throw @"compilationError" (Types.InternalFault "App on Michelson")
    -- Preconditition violated
    Ann.AppM {} → throw @"compilationError" (Types.InternalFault "App after App")
    Ann.App {} → throw @"compilationError" (Types.InternalFault "No Single Apps")
    Ann.Lam {} → throw @"compilationError" (Types.InternalFault "No Single Lambdas")

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
  v ← traverse (protect . (inst >=> promoteTopStack)) instrs
  case v of
    instr2 : instr1 : _ → do
      let instrs = [instr2, instr1]
      res ←
        if
          | allConstants (val <$> instrs) →
            let Env.Constant i1 = val instr1
                Env.Constant i2 = val instr2
             in pure (f i1 i2)
          | otherwise → do
            traverse_ addExpanded instrs
            addInstr op
            pure (Env.Expanded op)
      modify @"stack" (VStack.drop 2)
      consVal res undefined
      pure res
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

-- Promoting types happen elsewhere
-- so protect just serves to hold the ops effects
addExpanded ∷ Env.Ops m ⇒ Protect → m ()
addExpanded (Protect _ i) = addInstrs i

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

expandedToStack ∷ Env.Expanded → VStack.Val Env.Curr
expandedToStack (Env.Constant v) = VStack.ConstE v
expandedToStack (Env.Expanded _) = VStack.FuncResultE
expandedToStack (Env.Curr curry) = VStack.LamPartialE curry

consVal ∷ HasState "stack" (VStack.T Env.Curr) m ⇒ Env.Expanded → Types.Type → m ()
consVal result type' =
  modify @"stack" $
    VStack.cons
      ( VStack.Val
          (expandedToStack result),
        typeToPrimType type'
      )

consVar ∷
  HasState "stack" (VStack.T Env.Curr) m ⇒ Symbol → Env.Expanded → Usage.T → Types.Type → m ()
consVar symb result usage type' =
  modify @"stack" $
    VStack.cons
      ( VStack.VarE
          (Set.singleton symb)
          usage
          (Just (expandedToStack result)),
        typeToPrimType type'
      )

typeToPrimType ∷ Types.Type → Untyped.Type
typeToPrimType = undefined

promoteTopStack = undefined
