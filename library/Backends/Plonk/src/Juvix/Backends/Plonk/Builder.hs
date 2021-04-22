module Juvix.Backends.Plonk.Builder where

import Juvix.Backends.Plonk.Circuit
import Juvix.Backends.Plonk.IR
import Juvix.Library

-------------------------------------------------------------------------------
-- Circuit Builder
-------------------------------------------------------------------------------

data S f = S {sCircuit :: ArithCircuit f, sVarNum :: Int}
  deriving (Show, Generic)

newtype IRM f a = IRM {runIRM :: State (S f) a}
  deriving (Functor, Applicative, Monad)
  deriving
    ( HasState "sCircuit" (ArithCircuit f),
      HasSink "sCircuit" (ArithCircuit f),
      HasSource "sCircuit" (ArithCircuit f)
    )
    via StateField "sCircuit" (State (S f))
  deriving
    ( HasState "sVarNum" Int,
      HasSink "sVarNum" Int,
      HasSource "sVarNum" Int
    )
    via StateField "sVarNum" (State (S f))

execCircuitBuilder :: IRM f a -> ArithCircuit f
execCircuitBuilder m = reverseCircuit . sCircuit $ execState (runIRM m) (S (ArithCircuit []) 0)
  where
    reverseCircuit = \(ArithCircuit cs) -> ArithCircuit $ reverse cs

evalCircuitBuilder :: IRM f a -> a
evalCircuitBuilder = fst . runCircuitBuilder

runCircuitBuilder :: IRM f a -> (a, ArithCircuit f)
runCircuitBuilder m = second (reverseCircuit . sCircuit) $ runState (runIRM m) (S (ArithCircuit []) 0)
  where
    reverseCircuit = \(ArithCircuit cs) -> ArithCircuit $ reverse cs

fresh :: IRM f Int
fresh = do
  v <-
    get
      @"sVarNum"
  modify
    @"sVarNum"
    (+ 1)
  pure v

-- | Fresh intermediate variables
imm :: IRM f Wire
imm = IntermediateWire <$> fresh

-- | Fresh input variables
freshInput :: IRM f Wire
freshInput = InputWire <$> fresh

-- | Fresh output variables
freshOutput :: IRM f Wire
freshOutput = OutputWire <$> fresh

-- | Multiply two wires or affine circuits to an intermediate variable
mulToImm :: Either Wire (AffineCircuit Wire f) -> Either Wire (AffineCircuit Wire f) -> IRM f Wire
mulToImm l r = do
  o <- imm
  emit $ MulGate (addVar l) (addVar r) o
  pure o

-- | Add a Mul and its output to the ArithCircuit
emit :: Gate Wire f -> IRM f ()
emit c =
  modify
    @"sCircuit"
    (\(ArithCircuit cs) -> ArithCircuit (c : cs))

-- | Rotate a list to the right
rotateList :: Int -> [a] -> [a]
rotateList steps x = take (length x) $ drop steps $ cycle x

-- | Turn a wire into an affine circuit, or leave it be
addVar :: Either Wire (AffineCircuit Wire f) -> AffineCircuit Wire f
addVar (Left w) = Var w
addVar (Right c) = c

-- | Turn an affine circuit into a wire, or leave it be
addWire :: Num f => Either Wire (AffineCircuit Wire f) -> IRM f Wire
addWire (Left w) = pure w
addWire (Right c) = do
  mulOut <- imm
  emit $ MulGate (ConstGate 1) c mulOut
  pure mulOut

compile :: Num f => IR Wire f ty -> IRM f (Either Wire (AffineCircuit Wire f))
compile ir = case ir of
  IConst n -> pure . Right $ ConstGate n
  IVar v -> pure . Left $ v
  IUnOp op e1 -> do
    e1Out <- compile e1
    case op of
      UNot -> pure . Right $ Add (ConstGate 1) (ScalarMul (-1) (addVar e1Out))
  IBinOp op e1 e2 -> do
    e1Out <- addVar <$> compile e1
    e2Out <- addVar <$> compile e2
    case op of
      BAdd -> pure . Right $ Add e1Out e2Out
      BMul -> do
        tmp1 <- mulToImm (Right e1Out) (Right e2Out)
        pure . Left $ tmp1
      -- SUB(x, y) = x + (-y)
      BSub -> pure . Right $ Add e1Out (ScalarMul (-1) e2Out)
      BAnd -> do
        tmp1 <- mulToImm (Right e1Out) (Right e2Out)
        pure . Left $ tmp1
      BOr -> do
        -- OR(input1, input2) = (input1 + input2) - (input1 * input2)
        tmp1 <- imm
        emit $ MulGate e1Out e2Out tmp1
        pure . Right $ Add (Add e1Out e2Out) (ScalarMul (-1) (Var tmp1))
      BXor -> do
        -- XOR(input1, input2) = (input1 + input2) - 2 * (input1 * input2)
        tmp1 <- imm
        emit $ MulGate e1Out e2Out tmp1
        pure . Right $ Add (Add e1Out e2Out) (ScalarMul (-2) (Var tmp1))
  ICompOp op lhs rhs -> do
    case op of
      -- EQ(lhs, rhs) = (lhs - rhs == 1)
      CEq -> do
        lhsSubRhs <- compile (IBinOp BSub lhs rhs)
        eqInWire <- addWire lhsSubRhs
        eqFreeWire <- imm
        eqOutWire <- imm
        emit $ EqualGate eqInWire eqFreeWire eqOutWire
        -- eqOutWire == 0 if lhs == rhs, so we need to return 1 -
        -- neqOuti instead.
        pure . Right $ Add (ConstGate 1) (ScalarMul (-1) (Var eqOutWire))
  -- IF(cond, true, false) = (cond*true) + ((!cond) * false)
  IIf cond true false -> do
    condOut <- addVar <$> compile cond
    trueOut <- addVar <$> compile true
    falseOut <- addVar <$> compile false
    tmp1 <- imm
    tmp2 <- imm
    emit $ MulGate condOut trueOut tmp1
    emit $ MulGate (Add (ConstGate 1) (ScalarMul (-1) condOut)) falseOut tmp2
    pure . Right $ Add (Var tmp1) (Var tmp2)

-- | Translate an arithmetic expression to an arithmetic circuit
irToArithCircuit ::
  Num f =>
  -- | expression to compile
  IR Int f ty ->
  -- | Wire to assign the output of the expression to
  Wire ->
  IRM f ()
irToArithCircuit ir output =
  irToArithCircuit' (mapVarsIR InputWire ir) output

irToArithCircuit' :: Num f => IR Wire f ty -> Wire -> IRM f ()
irToArithCircuit' ir output = do
  irOut <- compile ir
  emit $ MulGate (ConstGate 1) (addVar irOut) output

-- | Apply function to variable names.
mapVarsIR :: (i -> j) -> IR i f ty -> IR j f ty
mapVarsIR f ir = case ir of
  IVar i -> IVar $ f i
  IConst v -> IConst v
  IBinOp op e1 e2 -> IBinOp op (mapVarsIR f e1) (mapVarsIR f e2)
  IUnOp op e1 -> IUnOp op (mapVarsIR f e1)
  IIf b tr fl -> IIf (mapVarsIR f b) (mapVarsIR f tr) (mapVarsIR f fl)

-- EEq lhs rhs -> EEq (mapVarsIR f lhs) (mapVarsIR f rhs)
