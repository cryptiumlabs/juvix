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

import Data.Maybe (fromJust)
import qualified Data.Set as Set
import qualified Debug.Trace as Trace
import qualified Juvix.Backends.Michelson.Compilation.Types as Types
import qualified Juvix.Backends.Michelson.Compilation.VirtualStack as VStack
import qualified Juvix.Backends.Michelson.DSL.Environment as Env
import qualified Juvix.Backends.Michelson.DSL.Instructions as Instructions
import qualified Juvix.Backends.Michelson.DSL.Untyped as Untyped
import qualified Juvix.Backends.Michelson.DSL.Utils as Utils
import qualified Juvix.Core.ErasedAnn.Types as Ann
import qualified Juvix.Core.Usage as Usage
import Juvix.Library hiding (abs, and, or, xor)
import qualified Juvix.Library (abs)
import qualified Michelson.Untyped.Instr as Instr
import qualified Michelson.Untyped.Type as MT
import qualified Michelson.Untyped.Value as V
import Prelude (error)

--------------------------------------------------------------------------------
-- Main Functionality
--------------------------------------------------------------------------------

instOuter :: Env.Reduction m => Types.NewTerm -> m Instr.ExpandedOp
instOuter a@(Types.Ann _ ty _) = do
  inst <- inst a
  ty <- typeToPrimType ty
  expandedToInst ty inst

expandedToInst :: Env.Reduction m => Untyped.T -> Env.Expanded -> m Instr.ExpandedOp
expandedToInst ty exp =
  case exp of
    Env.Constant c -> do
      let newInstr = VStack.constToInstr ty c
      addInstr newInstr
      pure newInstr
    Env.Expanded op -> pure op
    -- TODO
    Env.MichelsonLam -> error "fails on michLambda"
    Env.Curr c -> mconcat |<< promoteLambda c
    Env.Nop -> pure (Instr.SeqEx [])

inst :: Env.Reduction m => Types.NewTerm -> m Env.Expanded
inst (Types.Ann _usage ty t) =
  case t of
    Ann.Var symbol -> var symbol
    Ann.AppM fun a -> appM fun a
    Ann.LamM c a b -> do
      v <- lambda c a b ty
      consVal v ty
      pure v
    Ann.Prim prim' ->
      case prim' of
        Types.Inst _ -> constructPrim prim' ty
        Types.Constant m -> do
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
  or,
  car,
  cdr,
  pair ::
    Env.Reduction m => Types.Type -> [Types.NewTerm] -> m Env.Expanded
add = intGen Instructions.add (+)
mul = intGen Instructions.mul (*)
sub = intGen Instructions.sub (-)
and = onBoolGen Instructions.and (&&)
xor = onBoolGen Instructions.xor (/=)
or = onBoolGen Instructions.or (||)
ediv =
  onIntGen
    Instructions.ediv
    ( \x y -> case y of
        0 -> V.ValueNone
        y -> V.ValueSome (V.ValuePair (V.ValueInt (x `div` y)) (V.ValueInt (rem x y)))
    )
eq = onInt1 Instructions.eq (boolToVal . (== 0))
neq = onInt1 Instructions.neq (boolToVal . (/= 0))
le = onInt1 Instructions.le (boolToVal . (<= 0))
lt = onInt1 Instructions.lt (boolToVal . (< 0))
gt = onInt1 Instructions.ge (boolToVal . (>= 0))
ge = onInt1 Instructions.gt (boolToVal . (> 0))
neg = intGen1 Instructions.neg negate
abs = intGen1 Instructions.abs Juvix.Library.abs
car = onPairGen1 Instructions.car fst
cdr = onPairGen1 Instructions.cdr snd
pair = onTwoArgs Instructions.pair (Env.Constant ... V.ValuePair)
isNat =
  onInt1
    Instructions.isNat
    (\x -> if x >= 0 then V.ValueSome (V.ValueInt x) else V.ValueNone)

lambda :: Env.Error m => [Symbol] -> [Symbol] -> Types.Term -> Types.Type -> m Env.Expanded
lambda captures arguments body type'
  -- >= as we may return a lambda!
  | length usages >= length arguments =
    pure $
      Env.Curr
        Env.C
          { Env.captures = Set.fromList captures,
            Env.argsLeft = annotatedArgs,
            Env.left = fromIntegral (length arguments),
            Env.ty = type',
            Env.fun = Env.Fun (const (inst body <* traverse_ deleteVar annotatedArgs))
          }
  | otherwise =
    throw @"compilationError" Types.InvalidInputType
  where
    usages =
      Utils.usageFromType type'
    annotatedArgs =
      zipWith Env.Term arguments usages

var :: (Env.Instruction m, Env.Error m) => Symbol -> m Env.Expanded
var symb = do
  stack <- get @"stack"
  let pushStack value =
        case VStack.lookupType symb stack of
          Just t ->
            modify @"stack"
              ( VStack.cons (VStack.var1E symb (Just value), t)
                  . VStack.predValueUsage symb
              )
          Nothing ->
            throw @"compilationError" (Types.NotInStack symb)
  case VStack.lookup symb stack of
    Nothing ->
      throw @"compilationError" (Types.NotInStack symb)
    Just (VStack.Value (VStack.Val' value)) -> do
      pushStack (VStack.ConstE value)
      pure (Env.Constant value)
    Just (VStack.Value (VStack.Lam' lamPartial)) -> do
      pushStack (VStack.LamPartialE lamPartial)
      pure (Env.Curr lamPartial)
    Just (VStack.Position usage index)
      | one == usage ->
        Env.Expanded <$> moveToFront index
      | otherwise ->
        Env.Expanded <$> dupToFront index

-- Replaced to always just replace the top element
-- it seems all forms place a lambda at the top!

-- We ignore the usage of the term coming in, however if it's already
-- named, we don't actually change it's usage

-- |
-- Name calls inst, and then determines how best to name the form in the VStack
name :: Env.Reduction m => Env.ErasedTerm -> Types.NewTerm -> m Env.Expanded
name (Env.Term symb usage) f =
  inst f <* modify @"stack" (VStack.nameTop symb usage)

nameSymb :: Env.Reduction m => Symbol -> Types.NewTerm -> m Env.Expanded
nameSymb symb f@(Types.Ann usage _ _) =
  inst f <* modify @"stack" (VStack.nameTop symb usage)

primToFargs :: Num b => Types.NewPrim -> Types.Type -> (Env.Fun, b)
primToFargs (Types.Constant (V.ValueLambda _lam)) _ty =
  (undefined, 1)
primToFargs (Types.Inst inst) ty =
  case inst of
    -- Can't abstract out pattern due to bad forall resolution!
    Instr.ADD _ -> (Env.Fun (add newTy2), 2)
    Instr.SUB _ -> (Env.Fun (sub newTy2), 2)
    Instr.MUL _ -> (Env.Fun (mul newTy2), 2)
    Instr.OR {} -> (Env.Fun (or newTy2), 2)
    Instr.AND _ -> (Env.Fun (and newTy2), 2)
    Instr.XOR _ -> (Env.Fun (xor newTy2), 2)
    Instr.EQ {} -> (Env.Fun (eq newTy1), 1)
    Instr.NEQ _ -> (Env.Fun (neq newTy1), 1)
    Instr.LT {} -> (Env.Fun (lt newTy1), 1)
    Instr.LE {} -> (Env.Fun (le newTy1), 1)
    Instr.GE {} -> (Env.Fun (ge newTy1), 1)
    Instr.GT {} -> (Env.Fun (gt newTy1), 1)
    Instr.NEG _ -> (Env.Fun (neg newTy1), 1)
    Instr.ABS _ -> (Env.Fun (abs newTy1), 1)
    Instr.CAR {} -> (Env.Fun (car newTy1), 1)
    Instr.CDR {} -> (Env.Fun (cdr newTy1), 1)
    Instr.PAIR {} -> (Env.Fun (pair newTy2), 2)
    Instr.EDIV _ -> (Env.Fun (ediv newTy2), 2)
    Instr.ISNAT _ -> (Env.Fun (isNat newTy1), 1)
    Instr.PUSH {} -> (Env.Fun pushConstant, 1)
  where
    newTy i = eatType i ty
    newTy2 = newTy 2
    newTy1 = newTy 1
primToFargs (Types.Constant _) _ =
  error "Tried to apply a Michelson Constant"

appM :: Env.Reduction m => Types.NewTerm -> [Types.NewTerm] -> m Env.Expanded
appM form@(Types.Ann _u ty t) args =
  let app = inst form >>= flip applyExpanded args
   in case t of
        -- We could remove this special logic, however it would
        -- result in inefficient Michelson!
        Ann.Prim p ->
          let (f, lPrim) = primToFargs p ty
           in case length args `compare` lPrim of
                EQ -> Env.unFun f args
                LT -> app
                GT ->
                  throw
                    @"compilationError"
                    (Types.InternalFault "Michelson call with too many args")
        _ -> app

applyExpanded :: Env.Reduction m => Env.Expanded -> [Types.NewTerm] -> m Env.Expanded
applyExpanded expanded args = do
  (elem, ty) <- VStack.car |<< get @"stack"
  case expanded of
    Env.Curr c -> do
      modify @"stack" (VStack.drop 1)
      apply c args []
    -- We may get a Michelson lambda if we have one
    -- in storage, make sure to handle this case!
    Env.MichelsonLam -> do
      let VStack.VarE sym _ (Just VStack.MichelsonLambda) = elem
      applyLambdaFromStorageNArgs (Set.findMin sym) ty args
    Env.Constant _ -> throw @"compilationError" (Types.InternalFault "App on Constant")
    Env.Expanded _ -> throw @"compilationError" (Types.InternalFault "App on Michelson")
    Env.Nop -> throw @"compilationError" (Types.InternalFault "App on NOP")

--------------------------------------------------------------------------------
-- Reduction Helpers for Main functionality
--------------------------------------------------------------------------------

type OnTerm m f =
  Env.Reduction m =>
  Instr.ExpandedOp ->
  f ->
  Types.Type ->
  [Types.NewTerm] ->
  m Env.Expanded

type OnTerm2 m input result =
  OnTerm m (input -> input -> result)

type OnTerm1 m input result =
  OnTerm m (input -> result)

onBoolGen :: OnTerm2 m Bool Bool
onBoolGen op f =
  onTwoArgs
    op
    ( \instr1 instr2 ->
        let i1 = valToBool instr1
            i2 = valToBool instr2
         in Env.Constant (boolToVal (f i1 i2))
    )

intGen :: OnTerm2 m Integer Integer
intGen op f = onIntGen op (\x y -> V.ValueInt (f x y))

intGen1 :: OnTerm1 m Integer Integer
intGen1 op f = onInt1 op (V.ValueInt . f)

onInt1 :: OnTerm1 m Integer (V.Value' Types.Op)
onInt1 op f =
  onOneArgs
    op
    ( \instr1 ->
        let V.ValueInt i1 = instr1
         in Env.Constant (f i1)
    )

onIntGen :: OnTerm2 m Integer (V.Value' Types.Op)
onIntGen op f =
  onTwoArgs
    op
    ( \instr1 instr2 ->
        let V.ValueInt i1 = instr1
            V.ValueInt i2 = instr2
         in Env.Constant (f i1 i2)
    )

onPairGen1 :: OnTerm1 m (V.Value' Types.Op, V.Value' Types.Op) (V.Value' Types.Op)
onPairGen1 op f =
  onOneArgs
    op
    ( \instr1 ->
        let V.ValuePair car cdr = instr1
         in Env.Constant (f (car, cdr))
    )

onTwoArgs :: OnTerm2 m (V.Value' Types.Op) Env.Expanded
onTwoArgs op f typ instrs = do
  v <- traverse (protect . (inst >=> promoteTopStack)) (reverse instrs)
  case v of
    instr2 : instr1 : _ -> do
      -- May be the wrong order?
      let instrs = [instr2, instr1]
      res <-
        if  | allConstants (val <$> instrs) ->
              let Env.Constant i1 = val instr1
                  Env.Constant i2 = val instr2
               in pure (f i1 i2)
            | otherwise -> do
              traverse_ addExpanded instrs
              addInstr op
              pure Env.Nop
      modify @"stack" (VStack.drop 2)
      consVal res typ
      pure res
    _ -> throw @"compilationError" Types.NotEnoughArguments

onOneArgs :: OnTerm1 m (V.Value' Types.Op) Env.Expanded
onOneArgs op f typ instrs = do
  v <- traverse (protect . (inst >=> promoteTopStack)) instrs
  case v of
    instr1 : _ -> do
      res <-
        if  | allConstants [val instr1] ->
              let Env.Constant i1 = val instr1
               in pure (f i1)
            | otherwise -> do
              addExpanded instr1
              addInstr op
              pure Env.Nop
      modify @"stack" (VStack.drop 1)
      consVal res typ
      pure res
    _ -> throw @"compilationError" Types.NotEnoughArguments

-- todo remove repeat pattern
pushConstant :: Env.Reduction m => [Types.NewTerm] -> m Env.Expanded
pushConstant instrs = do
  v <- traverse (inst >=> promoteTopStack) instrs
  case v of
    Env.Constant _ : _ ->
      pure Env.Nop
    instr1 : _ -> pure instr1
    _ -> throw @"compilationError" Types.NotEnoughArguments

-------------------------------------------------------------------------------
-- Environment Protections, Promotions, and Movements
--------------------------------------------------------------------------------

moveToFront :: (Env.Instruction m, Integral a) => a -> m Instr.ExpandedOp
moveToFront num = do
  let inst = Instructions.dig (fromIntegral num)
  addInstr inst
  modify @"stack" (VStack.dig (fromIntegral num))
  pure inst

dupToFront :: (Env.Instruction m, Integral a) => a -> m Instr.ExpandedOp
dupToFront num = do
  modify @"stack" (VStack.dupDig (fromIntegral num))
  -- Since the instructions run in reverse if we add it this way
  -- we put dug first
  let instrs =
        Instructions.dig (fromIntegral num)
          <> Instructions.dup
          <> Instructions.dug (succ (fromIntegral num))
  addInstr instrs
  pure instrs

data Protect
  = Protect
      { val :: Env.Expanded,
        insts :: [Types.Op]
      }
  deriving (Show)

protect :: Env.Ops m => m Env.Expanded -> m Protect
protect inst = do
  curr <- get @"ops"
  -- Clear the ops, so we only capture the new ops
  put @"ops" []
  v <- inst
  after <- get @"ops"
  put @"ops" curr
  pure Protect {val = v, insts = after}

data ProtectStack
  = ProtectStack
      { prot :: Protect,
        stack :: VStack.T Env.Curried
      }
  deriving (Show)

protectStack :: Env.Instruction m => m Env.Expanded -> m ProtectStack
protectStack inst = do
  curr <- get @"stack"
  prot <- protect inst
  after <- get @"stack"
  put @"stack" curr
  pure ProtectStack {prot = prot, stack = after}

-- Promoting types happen elsewhere
-- so protect just serves to hold the ops effects
addExpanded :: Env.Ops m => Protect -> m ()
addExpanded (Protect _ i) = addInstrs i

promoteTopStack :: Env.Reduction m => Env.Expanded -> m Env.Expanded
promoteTopStack x = do
  stack <- get @"stack"
  (insts, stack') <- VStack.promote 1 stack promoteLambda
  put @"stack" stack'
  addInstrs insts
  pure x

reserveNames :: HasState "count" Word m => Word -> m [Symbol]
reserveNames i = do
  c <- get @"count"
  put @"count" (i + c)
  pure (intern . show <$> [c .. c + i - 1])

-- TODO ∷ drop extra things from the vstack, mainly
-- 1. the function we move to the front
-- 2. the expanded lambda we create
-- Note that it isn't vital, as these aren't stored in the real Michelson stack
-- Other things considered:
-- We don't need to drop the arguments we eval and name, as they should be eaten
-- by the functions they call with the appropriate usages
apply :: Env.Reduction m => Env.Curried -> [Types.NewTerm] -> [Symbol] -> m Env.Expanded
apply closure args remainingArgs = do
  let totalLength = fromIntegral (length args + length remainingArgs)
  case totalLength `compare` Env.left closure of
    EQ -> do
      -- usage and type doesn't matter here!
      evalArgsAndName
      app
    LT -> do
      evalArgsAndName
      -- make new closure,
      let remaining = Env.left closure - totalLength
          (captured, left) = splitAt (fromIntegral totalLength) (Env.argsLeft closure)
          captureNames = fmap Env.name captured
          tyList = take (length captured) (Utils.piToListTy (Env.ty closure))
          con =
            Env.C
              { Env.left = remaining,
                Env.argsLeft = left,
                Env.captures = foldr Set.insert (Env.captures closure) captureNames,
                Env.ty = eatType (fromIntegral totalLength) (Env.ty closure),
                Env.fun = Env.Fun $ \args ->
                  Env.unFun
                    (Env.fun closure)
                    (args <> zipWith makeVar (reverse captured) (reverse tyList))
              }
      consVal (Env.Curr con) (Env.ty con)
      pure (Env.Curr con)
    -- So in this case, we have
    --  | args + remainingArgs | > left/|argsLeft|
    -- We thus need to determine, the args left to eval, do we have enough
    -- names for them?
    -- If so then we can just name them all without reserving any extra names
    -- however if we can't then we need to reserve names first then eval!
    GT ->
      -- figure out which arguments go unnamed, name them
      -- then carry it over to the recursion
      case fromIntegral (length args) `compare` Env.left closure of
        -- This case we have | args | = left\|argsLeft|, so we
        -- can just eval the args and name them, we know, none of the
        -- remaining args get a new name nor are applied, so carry over to
        -- next application
        EQ -> do
          evalArgsAndName
          expanded <- app
          recur expanded remainingArgs
        -- Here we have | args | < left, so we know we have enough names
        -- to name them all, there will be a few names that spill over to
        -- the remaining args, this number is `left - | args |`
        -- Example ∷ argsLeft = [_1, _2]. args = [3], remaining = [_4,_5, ...]
        -- evalArgsAndName ==> [_1 = 3, _2 = _4], but we have [_5, ... remaining
        -- drop (2 - 1) [_4, _5, ...] = [_5 ...]
        LT -> do
          let notRemaining = fromIntegral (Env.left closure) - length args
              newRemaining = drop notRemaining remainingArgs
          evalArgsAndName
          expanded <- app
          recur expanded newRemaining
        -- In this case, we lack enough names for arguments since | args | > left
        -- So we need to see how many arguments are left, this is `| args | - left`
        -- we then need to reserve that many, eval right to left, then append the args
        -- left to the remaining args, since they too need to be applied
        GT -> do
          let unNamed = drop (fromIntegral $ Env.left closure) args
          moreReserved <- reserveNames (fromIntegral (length unNamed))
          traverseNameSymb (zip moreReserved unNamed)
          traverseName (zip (Env.argsLeft closure) args)
          expanded <- app
          recur expanded (moreReserved <> remainingArgs)
  where
    evalArgsAndName = do
      let (toEvalNames, alreadyEvaledNames) = splitAt (length args) (Env.argsLeft closure)
      traverseName (zip toEvalNames args)
      traverse_
        (modify @"stack" . uncurry VStack.addName)
        (zip remainingArgs (Env.name <$> alreadyEvaledNames))
    traverseName = traverse_ (uncurry name) . reverse
    traverseNameSymb = traverse_ (uncurry nameSymb) . reverse
    app =
      Env.unFun
        (Env.fun closure)
        -- Undo our last flip, and put the list in the proper place
        $ reverse
        $ zipWith makeVar (reverse (Env.argsLeft closure))
        $ reverse
        $ Utils.piToListTy
        $ Env.ty closure
    makeVar (Env.Term name usage) ty = Types.Ann usage ty (Ann.Var name)
    -- TODO ∷ see if we have to drop the top of the stack? It seems to be handled?
    recur (Env.Curr c) xs =
      apply c [] xs
    -- Exec case for Michelson lambdas, e.g. if (storage = f) → g f = f 3!
    recur Env.MichelsonLam _xs =
      applyLambdaFromStorageNArgs undefined undefined args
    recur (Env.Constant _) _ =
      throw @"compilationError" (Types.InternalFault "apply to non lam")
    recur (Env.Expanded _) _ =
      throw @"compilationError" (Types.InternalFault "apply to non lam")
    recur Env.Nop _ =
      throw @"compilationError" (Types.InternalFault "apply to non lam")

-- we can only delete things at position greater than 0.
-- this is because if we were to delete 0, then (λx : ω i. x) would error
deleteVar :: Env.Instruction m => Env.ErasedTerm -> m ()
deleteVar (Env.Term name _usage) = do
  stack <- get @"stack"
  let pos = VStack.lookupAllPos name stack
      op i = do
        addInstr (Instructions.dipN (fromIntegral i) [Instructions.drop])
        modify @"stack" (VStack.dropPos i)
      f (VStack.Value _) = pure ()
      f (VStack.Position _ 0) = do
        stack <- get @"stack"
        if  | VStack.constantOnTop stack ->
              op 0
            | otherwise ->
              pure ()
      f (VStack.Position _ index) =
        op index
  -- reverse is important here, as we don't decrease the pos of upcoming terms
  -- note that this should only ever hit at most 2 elements, so the
  -- cost of this slightly inefficient generation does not ever matter!
  traverse_ f (reverse pos)

--------------------------------------------------------------------------------
-- Effect Wrangling
--------------------------------------------------------------------------------

addInstrs :: Env.Ops m => [Instr.ExpandedOp] -> m ()
addInstrs x = modify @"ops" (<> x)

addInstr :: Env.Ops m => Instr.ExpandedOp -> m ()
addInstr x = modify @"ops" (<> [x])

--------------------------------------------------------------------------------
-- Boolean Conversions
--------------------------------------------------------------------------------

boolToVal :: Bool -> V.Value' op
boolToVal True = V.ValueTrue
boolToVal False = V.ValueFalse

valToBool :: V.Value' op -> Bool
valToBool V.ValueTrue = True
valToBool V.ValueFalse = False
valToBool _ = error "called valToBool on a non Michelson Bool"

--------------------------------------------------------------------------------
-- Misc
--------------------------------------------------------------------------------

-- TODO ∷ determine if we'd ever call this on a constant like primitive
constructPrim ::
  (Env.Stack m, Env.Count m, Env.Error m) => Types.NewPrim -> Types.Type -> m Env.Expanded
constructPrim prim ty = do
  let (f, lPrim) = primToFargs prim ty
  names <- reserveNames lPrim
  -- TODO ∷ set the usage of the arguments to 1
  let c =
        Env.Curr $
          Env.C
            { Env.fun = f,
              Env.argsLeft = zipWith Env.Term names (repeat one),
              Env.left = fromIntegral lPrim,
              Env.captures = Set.empty,
              Env.ty = ty
            }
  consVal c ty
  pure c

allConstants :: [Env.Expanded] -> Bool
allConstants = all f
  where
    f (Env.Constant _) = True
    f (Env.Expanded _) = False
    f Env.MichelsonLam = False
    f Env.Curr {} = True
    f Env.Nop = False

expandedToStack :: Env.Expanded -> VStack.Val Env.Curried
expandedToStack (Env.Constant v) = VStack.ConstE v
expandedToStack (Env.Expanded _) = VStack.FuncResultE
expandedToStack (Env.Curr curry) = VStack.LamPartialE curry
expandedToStack Env.MichelsonLam = VStack.MichelsonLambda
expandedToStack Env.Nop = VStack.FuncResultE

consVal :: (Env.Stack m, Env.Error m) => Env.Expanded -> Types.Type -> m ()
consVal result ty = do
  ty <- typeToPrimType ty
  modify @"stack" $
    VStack.cons
      ( VStack.Val
          (expandedToStack result),
        ty
      )

consVarGen ::
  ( HasThrow "compilationError" Env.CompError m,
    HasState "stack" (VStack.T lamType) m
  ) =>
  Symbol ->
  Maybe (VStack.Val lamType) ->
  Usage.T ->
  Types.Type ->
  m ()
consVarGen symb result usage ty = do
  ty <- typeToPrimType ty
  modify @"stack" $
    VStack.cons
      ( VStack.VarE
          (Set.singleton symb)
          usage
          result,
        ty
      )

consVar ::
  (Env.Stack m, Env.Error m) => Symbol -> Env.Expanded -> Usage.T -> Types.Type -> m ()
consVar symb result = consVarGen symb (Just (expandedToStack result))

consVarNone ::
  (Env.Stack m, Env.Error m) => Env.ErasedTerm -> Types.Type -> m ()
consVarNone (Env.Term symb usage) = consVarGen symb Nothing usage

typeToPrimType :: forall m. Env.Error m => Types.Type -> m Untyped.T
typeToPrimType ty =
  case ty of
    Ann.SymT _ -> throw @"compilationError" Types.InvalidInputType
    Ann.Star _ -> throw @"compilationError" Types.InvalidInputType
    Ann.PrimTy (Types.PrimTy mTy) -> pure mTy
    -- TODO ∷ Integrate usage information into this
    Ann.Pi _usages argTy retTy -> do
      argTy <- typeToPrimType argTy
      retTy <- typeToPrimType retTy
      pure (Untyped.lambda argTy retTy)

eatType :: Natural -> Types.Type -> Types.Type
eatType 0 t = t
eatType x (Ann.Pi _ _ a) = eatType (pred x) a
eatType _ _ = error "Only eat parts of a Pi types, not any other type!"

--------------------------------------------------------------------------------
-- Misc
--------------------------------------------------------------------------------

mustLookupType ::
  ( HasState "stack" (VStack.T lamType) m,
    HasThrow "compilationError" Types.CompilationError m
  ) =>
  Symbol ->
  m Untyped.T
mustLookupType sym = do
  stack <- get @"stack"
  case VStack.lookupType sym stack of
    Just ty -> pure ty
    Nothing -> throw @"compilationError" (Types.InternalFault "must be able to find type")

-- TODO ∷ figure out why we remove some of the bodies effects
promoteLambda :: Env.Reduction m => Env.Curried -> m [Instr.ExpandedOp]
promoteLambda (Env.C fun argsLeft left captures ty) = do
  -- Step 1: Copy the captures to the top of the stack.
  let capturesList = Set.toList captures
  -- TODO ∷ Figure out how to properly deal with usages here.
  capturesInsts <- flip traverse capturesList $ \c -> do
    ty <- mustLookupType c
    instr <- var c
    pure (ty, instr)
  curr <- get @"stack"
  -- Step 2: Figure out what the stack will be in the body of the function.
  -- Note: these lets are dropping usages the lambda consumes.
  let listOfArgsType = Utils.piToListTy ty
      Just returnType = lastMay listOfArgsType
      termList = reverse $ zip listOfArgsType argsLeft
      stackLeft = VStack.take (length captures) curr
      noVirts = VStack.dropAllVirtual stackLeft
      numberOfExtraArgs = VStack.realItems noVirts
      -- Make sure to run before insts!
      -- Will end up with args ... : captures ... : [] on the stack.
      unpackOps
        | numberOfExtraArgs > 0 =
          Utils.unpackArgsCaptures (fromIntegral left) (fromIntegral numberOfExtraArgs)
        | otherwise =
          Utils.unpackTupleN (fromIntegral (pred left))
  p <- protectStack $ do
    put @"stack" stackLeft
    traverse_ (\(t, term) -> consVarNone term t) termList
    -- Step 3: Compile the body of the lambda.
    insts <-
      -- TODO Clean this up with a helper!
      Env.unFun fun (fmap (\(t, Env.Term sym u) -> Types.Ann u t (Ann.Var sym)) termList)
    returnTypePrim <- typeToPrimType returnType
    _insts <- expandedToInst returnTypePrim insts
    modify @"ops" (unpackOps :)
    pure Env.MichelsonLam
  case p of
    ProtectStack (Protect _val insts) _stack -> do
      -- Step 4: Pack up the captures.
      capturesInsts <- mapM (uncurry expandedToInst) capturesInsts
      -- TODO ∷ Reduce usages of the vstack items, due to eating n from the lambda.
      -- Step 5: find the types of the captures, and generate the type for primArg
      argsWithTypes <- mapM (\(ty, Env.Term sym _u) -> typeToPrimType ty >>| (,) sym) termList
      primReturn <- typeToPrimType returnType
      let capturesTypes =
            (\x -> (x, fromJust (VStack.lookupType x curr)))
              <$> VStack.symbolsInT capturesList curr
          argTy = Utils.lamType capturesTypes argsWithTypes primReturn
          -- Step 6: generate the lambda
          -- TODO ∷ maybe reverse the instrs to lambda that isn't the car cdr dip stuff?
          lambda = Instructions.lambda argTy primReturn insts
      -- Return all operations in order: push the lambda, evaluate captures, pair up captures, APPLY.
      modify @"ops" (lambda :)
      when (numberOfExtraArgs > 0) $
        modify @"ops"
          ( [ Instructions.dip capturesInsts,
              Utils.pairN (numberOfExtraArgs - 1),
              Instructions.apply
            ]
              <>
          )
      get @"ops"

-- Assume lambdas from storage are curried.
applyLambdaFromStorage :: Env.Reduction m => Symbol -> Types.Type -> Types.NewTerm -> m [Instr.ExpandedOp]
applyLambdaFromStorage sym ty arg = do
  ty' <- typeToPrimType ty
  lam <- expandedToInst ty' =<< var sym
  arg <- instOuter arg
  ty <- typeToPrimType (eatType 1 ty)
  modify @"stack" (VStack.cons (VStack.varNone "_", ty) . VStack.drop 2)
  pure [lam, arg, Instructions.exec]

applyLambdaFromStorageNArgs :: Env.Reduction m => Symbol -> MT.Type -> [Types.NewTerm] -> m Env.Expanded
applyLambdaFromStorageNArgs sym ty args = Env.Expanded . mconcat |<< do
  undefined
