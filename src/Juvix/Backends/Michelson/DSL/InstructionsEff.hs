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
import Juvix.Library hiding (abs, and, or, xor)
import qualified Juvix.Library (abs)
import qualified Michelson.Untyped.Instr as Instr
import qualified Michelson.Untyped.Type as Untyped
import qualified Michelson.Untyped.Value as V
import Prelude (error)

--------------------------------------------------------------------------------
-- Main Functionality
--------------------------------------------------------------------------------

inst ∷ Env.Reduction m ⇒ Types.NewTerm → m Env.Expanded
inst (t, _usage, ty) =
  case t of
    Ann.Var symbol → var symbol
    Ann.LamM c a b → let v = (lambda c a b ty) in v <$ consVal v ty
    Ann.AppM fun a → appM fun a
    Ann.Prim prim' →
      case prim' of
        Types.Inst _ → constructPrim prim' ty
        Types.Constant m → do
          consVal (Env.Constant m) ty
          pure (Env.Constant m)

add,
  mul,
  sub,
  ediv,
  and,
  xor,
  neq,
  eq,
  lt,
  gt,
  le,
  ge,
  neg,
  abs,
  isNat,
  or ∷
    Env.Reduction m ⇒ Types.Type → [Types.NewTerm] → m Env.Expanded
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
eq = onInt1 Instructions.eq (boolToVal . (== 0))
neq = onInt1 Instructions.neq (boolToVal . (/= 0))
le = onInt1 Instructions.le (boolToVal . (<= 0))
lt = onInt1 Instructions.lt (boolToVal . (< 0))
gt = onInt1 Instructions.ge (boolToVal . (>= 0))
ge = onInt1 Instructions.gt (boolToVal . (> 0))
neg = intGen1 Instructions.neg negate
abs = intGen1 Instructions.abs Juvix.Library.abs
isNat =
  onInt1
    Instructions.isNat
    (\x → if x >= 0 then V.ValueSome (V.ValueInt x) else V.ValueNone)

lambda ∷ [Symbol] → [Symbol] → Types.Term → Types.Type → Env.Expanded
lambda captures arguments body type' = Env.Curr Env.C
  { Env.captures = Set.fromList captures,
    Env.argsLeft = arguments,
    Env.left = fromIntegral (length arguments),
    Env.ty = type',
    Env.fun = Env.Fun (const (inst body))
  }

var ∷ (Env.Instruction m, Env.Error m) ⇒ Symbol → m Env.Expanded
var symb = do
  stack ← get @"stack"
  let pushStack value =
        case VStack.lookupType symb stack of
          Just t → do
            modify @"stack" (VStack.cons (VStack.var1E symb (Just value), t))
          Nothing →
            throw @"compilationError" (Types.NotInStack symb)
  case VStack.lookup symb stack of
    Nothing →
      throw @"compilationError" (Types.NotInStack symb)
    -- TODO ∷ reduce usage by 1
    Just (VStack.Value (VStack.Val' value)) → do
      pushStack (VStack.ConstE value)
      pure (Env.Constant value)
    Just (VStack.Value (VStack.Lam' lamPartial)) → do
      pushStack (VStack.LamPartialE lamPartial)
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
primToFargs ∷ Num b ⇒ Types.NewPrim → Types.Type → (Env.Fun, b)
primToFargs (Types.Constant (V.ValueLambda _lam)) _ty =
  (undefined, 1)
primToFargs (Types.Inst inst) ty =
  case inst of
    -- Can't abstract out pattern due to bad forall resolution!
    Instr.ADD _ → (Env.Fun (add ty), 2)
    Instr.SUB _ → (Env.Fun (sub ty), 2)
    Instr.MUL _ → (Env.Fun (mul ty), 2)
    Instr.OR {} → (Env.Fun (or ty), 2)
    Instr.AND _ → (Env.Fun (and ty), 2)
    Instr.XOR _ → (Env.Fun (xor ty), 2)
    Instr.EQ {} → (Env.Fun (eq ty), 1)
    Instr.NEQ _ → (Env.Fun (neq ty), 1)
    Instr.LT {} → (Env.Fun (lt ty), 1)
    Instr.LE {} → (Env.Fun (le ty), 1)
    Instr.GE {} → (Env.Fun (ge ty), 1)
    Instr.GT {} → (Env.Fun (gt ty), 1)
    Instr.NEG _ → (Env.Fun (neg ty), 1)
    Instr.ABS _ → (Env.Fun (abs ty), 1)
    Instr.EDIV _ → (Env.Fun (ediv ty), 2)
    Instr.ISNAT _ → (Env.Fun (isNat ty), 1)
primToFargs (Types.Constant _) _ =
  error "Tried to apply a Michelson Constant"

appM ∷ Env.Reduction m ⇒ Types.NewTerm → [Types.NewTerm] → m Env.Expanded
appM form@(t, _u, ty) args =
  case t of
    -- We could remove this special logic, however it would
    -- result in inefficient Michelson!
    Ann.Prim p →
      let (f, lPrim) = primToFargs p ty
          argsL = length args
       in case argsL `compare` lPrim of
            EQ → Env.unFun f args
            LT → do
              form ← inst form
              applyExpanded form args
            GT →
              throw
                @"compilationError"
                (Types.InternalFault "Michelson call with too many args")
    _ → do
      form ← inst form
      applyExpanded form args

applyExpanded ∷ Env.Reduction m ⇒ Env.Expanded → [Types.NewTerm] → m Env.Expanded
applyExpanded expanded args = do
  modify @"stack" (VStack.drop 1)
  case expanded of
    Env.Curr c → apply c args []
    -- these two cases would only be valid if we expanded into a Michelson
    -- lambda, however would we ever do that?
    Env.Constant _ → throw @"compilationError" (Types.InternalFault "App on Constant")
    Env.Expanded _ → throw @"compilationError" (Types.InternalFault "App on Michelson")

--------------------------------------------------------------------------------
-- Reduction Helpers for Main functionality
--------------------------------------------------------------------------------

type OnTerm m f =
  Env.Reduction m ⇒
  Instr.ExpandedOp →
  f →
  Types.Type →
  [Types.NewTerm] →
  m Env.Expanded

type OnTerm2 m input result =
  OnTerm m (input → input → result)

type OnTerm1 m input result =
  OnTerm m (input → result)

onBoolGen ∷ OnTerm2 m Bool Bool
onBoolGen op f =
  onTwoArgs
    op
    ( \instr1 instr2 →
        let i1 = valToBool instr1
            i2 = valToBool instr2
         in Env.Constant (boolToVal (f i1 i2))
    )

intGen ∷ OnTerm2 m Integer Integer
intGen op f = onIntGen op (\x y → V.ValueInt (f x y))

intGen1 ∷ OnTerm1 m Integer Integer
intGen1 op f = onInt1 op (\x → V.ValueInt (f x))

onInt1 ∷ OnTerm1 m Integer (V.Value' Types.Op)
onInt1 op f =
  onOneArgs
    op
    ( \instr1 →
        let V.ValueInt i1 = instr1
         in Env.Constant (f i1)
    )

onIntGen ∷ OnTerm2 m Integer (V.Value' Types.Op)
onIntGen op f =
  onTwoArgs
    op
    ( \instr1 instr2 →
        let V.ValueInt i1 = instr1
            V.ValueInt i2 = instr2
         in Env.Constant (f i1 i2)
    )

onTwoArgs ∷ OnTerm2 m (V.Value' Types.Op) Env.Expanded
onTwoArgs op f typ instrs = do
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
      consVal res typ
      pure res
    _ → throw @"compilationError" Types.NotEnoughArguments

onOneArgs ∷ OnTerm1 m (V.Value' Types.Op) Env.Expanded
onOneArgs op f typ instrs = do
  v ← traverse (protect . (inst >=> promoteTopStack)) instrs
  case v of
    instr1 : _ → do
      res ←
        if
          | allConstants [val instr1] →
            let Env.Constant i1 = val instr1
             in pure (f i1)
          | otherwise → do
            addExpanded instr1
            addInstr op
            pure (Env.Expanded op)
      modify @"stack" (VStack.drop 2)
      consVal res typ
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

promoteTopStack ∷ Env.Instruction m ⇒ Env.Expanded → m Env.Expanded
promoteTopStack x = do
  stack ← get @"stack"
  let (insts, stack') = VStack.promote 1 stack
  put @"stack" stack'
  addInstrs insts
  pure x

reserveNames ∷ HasState "count" Word m ⇒ Word → m [Symbol]
reserveNames i = do
  c ← get @"count"
  put @"count" (i + c)
  pure (intern . show <$> [c .. c + i - 1])

-- TODO ∷ drop extra things from the vstack, mainly
-- 1. the function we move to the front
-- 2. the expanded lambda we create
-- Note that it isn't vital, as these aren't stored in the real Michelson stack
-- Other things considered:
-- We don't need to drop the arguments we eval and name, as they should be eaten
-- by the functions they call with the appropriate usages
apply ∷ Env.Reduction m ⇒ Env.Curr → [Types.NewTerm] → [Symbol] → m Env.Expanded
apply closure args remainingArgs = do
  let totalLength = fromIntegral (length args + length remainingArgs)
  case totalLength `compare` Env.left closure of
    EQ → do
      -- usage and type doesn't matter here!
      evalArgsAndName
      app
    LT → do
      evalArgsAndName
      -- make new closure, and apply args to the fun in the closure
      -- allowing more arguments to come, in a cont style!
      let remaining = Env.left closure - totalLength

          (captured, left) = splitAt (fromIntegral totalLength) (Env.argsLeft closure)

          con = Env.C
            { Env.left = remaining,
              Env.argsLeft = left,
              Env.captures = foldr Set.insert (Env.captures closure) captured,
              Env.ty = eatType (fromIntegral totalLength) (Env.ty closure),
              Env.fun = Env.Fun $ \args →
                Env.unFun
                  (Env.fun closure)
                  (args <> fmap makeVar (reverse captured))
            }

      consVal (Env.Curr con) (Env.ty con)
      pure (Env.Curr con)
    -- So in this case, we have
    --  | args + remainingArgs | > left/|argsLeft|
    -- We thus need to determine, the args left to eval, do we have enough
    -- names for them?
    -- If so then we can just name them all without reserving any extra names
    -- however if we can't then we need to reserve names first then eval!
    GT →
      -- figure out which arguments go unnamed, name them
      -- then carry it over to the recursion
      case fromIntegral (length args) `compare` Env.left closure of
        -- This case we have | args | = left\|argsLeft|, so we
        -- can just eval the args and name them, we know, none of the
        -- remaining args get a new name nor are applied, so carry over to
        -- next application
        EQ → do
          evalArgsAndName
          expanded ← app
          recur expanded remainingArgs
        -- Here we have | args | < left, so we know we have enough names
        -- to name them all, there will be a few names that spill over to
        -- the remaining args, this number is `left - | args |`
        -- Example ∷ argsLeft = [_1, _2]. args = [3], remaining = [_4,_5, ...]
        -- evalArgsAndName ==> [_1 = 3, _2 = _4], but we have [_5, ... remaining
        -- drop (2 - 1) [_4, _5, ...] = [_5 ...]
        LT → do
          let notRemaining = fromIntegral (Env.left closure) - length args
              newRemaining = drop notRemaining remainingArgs
          evalArgsAndName
          expanded ← app
          recur expanded newRemaining
        -- In this case, we lack enough names for arguments since | args | > left
        -- So we need to see how many arguments are left, this is `| args | - left`
        -- we then need to reserve that many, eval right to left, then append the args
        -- left to the remaining args, since they too need to be applied
        GT → do
          let unNamed = drop (fromIntegral $ Env.left closure) args
          moreReserved ← reserveNames (fromIntegral (length unNamed))
          traverseName (zip moreReserved unNamed)
          traverseName (zip (Env.argsLeft closure) args)
          expanded ← app
          recur expanded (moreReserved <> remainingArgs)
  where
    evalArgsAndName = do
      let (toEvalNames, alreadyEvaledNames) = splitAt (length args) (Env.argsLeft closure)
      traverseName (zip toEvalNames args)
      traverse_
        (modify @"stack" . uncurry VStack.addName)
        (zip remainingArgs alreadyEvaledNames)

    traverseName = traverse_ (uncurry name) . reverse

    app =
      Env.unFun
        (Env.fun closure)
        (makeVar <$> reverse (Env.argsLeft closure))

    makeVar v = (Ann.Var v, one, Env.ty closure)

    recur (Env.Curr c) xs =
      apply c [] xs
    recur (Env.Constant _) _ =
      throw @"compilationError" (Types.InternalFault "apply to non lam")
    recur (Env.Expanded _) _ =
      throw @"compilationError" (Types.InternalFault "apply to non lam")

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

constructPrim ∷ (Env.Stack m, Env.Count m) ⇒ Types.NewPrim → Types.Type → m Env.Expanded
constructPrim prim ty = do
  let (f, lPrim) = primToFargs prim ty
  names ← reserveNames lPrim
  -- TODO ∷ set the usage of the arguments to 1
  let c = Env.Curr $ Env.C
        { Env.fun = f,
          Env.argsLeft = names,
          Env.left = fromIntegral lPrim,
          Env.captures = Set.empty,
          Env.ty = ty
        }
  consVal c ty
  pure c

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

eatType ∷ Natural → Types.Type → Types.Type
eatType 0 t = t
eatType x (Ann.Pi _ _ a) = eatType (pred x) a
eatType _ _ = error "Only eat parts of a Pi types, not any other type!"
