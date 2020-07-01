module Juvix.Backends.ArithmeticCircuit.Compilation
  ( compile,
    add,
    mul,
    sub,
    neg,
    eq,
    exp,
    int,
    c,
    and',
    or',
    Types.Term,
    Types.Type,
    lambda,
    var,
    input,
    cond,
    true,
    false,
    runCirc,
  )
where

import qualified Circuit
import qualified Circuit.Expr as Expr
import qualified Circuit.Lang as Lang
import qualified Data.Map ()
import qualified Juvix.Backends.ArithmeticCircuit.Compilation.Environment as Env
import qualified Juvix.Backends.ArithmeticCircuit.Compilation.Types as Types
import qualified Juvix.Backends.ArithmeticCircuit.Parameterisation as Par
import qualified Juvix.Core.ErasedAnn as CoreErased
import qualified Juvix.Core.Usage as Usage
import Juvix.Library hiding (Type, exp)
import Numeric.Natural ()

compile ::
  Types.Term ->
  Types.Type ->
  (Either Types.CompilationError Types.Expression, Circuit.ArithCircuit Par.F)
compile term _ =
  Env.Env mempty Types.NoExp
    |> runState (runExceptT (Env.antiAlias $ transTerm term))
    |> fmap toArithCircuit
  where
    toArithCircuit env =
      case Env.compilation env of
        Types.BoolExp exp -> runCirc exp
        Types.FExp exp -> runCirc exp
        Types.NoExp -> panic "Tried to compile an empty circuit"

runCirc :: Num f => Expr.Expr Circuit.Wire f ty -> Circuit.ArithCircuit f
runCirc = Expr.execCircuitBuilder . Expr.compile

transTerm :: Env.HasCompErr m => Types.Term -> m Types.Expression
transTerm erased =
  case CoreErased.term erased of
    CoreErased.Prim term -> transPrim term
    CoreErased.Var var -> do
      (n, exp) <- Env.lookup var
      case exp of
        Types.NoExp -> do
          _ <- Env.insert var (Types.FExp $ input n)
          Env.write (Types.FExp $ input n)
        _ ->
          Env.write exp
    CoreErased.LamM {body, arguments} -> do
      Env.freshVars arguments
      transTerm body
    CoreErased.AppM f params -> do
      case f of
        CoreErased.Ann {CoreErased.term = CoreErased.LamM {body, arguments}} -> do
          let execParams (sy, term) = transTerm term >>= Env.insert sy
          --
          traverse_ execParams (zip arguments params)
          term <- transTerm body
          -- We must remove variables introduced by current function
          traverse_ Env.remove arguments
          return term
        _ -> throw @"compilationError" Types.TypeErrorApplicationNonFunction

-- - _for the calls below FEInteger_
--   + Types.Expression cannot be made a Functor since the structure
--     inside it is not a Functor It is a Bifunctor where I need to fix
--     the last variant, making it a Contravariant Functor Therefore, we
--     are sticking to manual Applicative until we move away from
--     `arithmetic-circuits`
-- - _for the Types.BinOp Types.Exp_
--   + we implement exponentiation by hand
--     since `arithmetic-circuits` does not support it

transPrim :: Env.HasCompErr m => Types.PrimVal -> m Types.Expression
transPrim (Types.Element f) = Env.write . Types.FExp $ Lang.c f
transPrim (Types.Boolean b) = Env.write . Types.BoolExp $ Circuit.EConstBool b
transPrim (Types.FEInteger i) = Env.write . Types.FExp $ Lang.c (fromIntegral i)
--
transPrim (Types.BinOp Types.Add prim prim') = onTwoPrimF Lang.add prim prim'
transPrim (Types.BinOp Types.Mul prim prim') = onTwoPrimF Lang.mul prim prim'
transPrim (Types.BinOp Types.Sub prim prim') = onTwoPrimF Lang.sub prim prim'
transPrim (Types.BinOp Types.And prim prim') = onTwoPrimBool Lang.and_ prim prim'
transPrim (Types.BinOp Types.Eq prim prim') = onTwoPrimFEq Lang.eq prim prim'
transPrim (Types.BinOp Types.Or prim prim') = onTwoPrimBool Lang.or_ prim prim'
transPrim (Types.UnaryOp Types.Neg prim) = do
  prim1 <- transTerm prim
  case prim1 of
    Types.BoolExp prim1' ->
      Lang.not_ prim1'
        |> Types.BoolExp
        |> Env.write
    _ -> throw @"compilationError" Types.PrimTypeError
transPrim (Types.If prim prim' prim'') = do
  prim1 <- transTerm prim
  prim2 <- transTerm prim'
  prim3 <- transTerm prim''
  case (prim1, prim2, prim3) of
    (Types.BoolExp prim1', Types.FExp prim2', Types.FExp prim3') ->
      (Env.write . Types.FExp) $ Lang.cond prim1' prim2' prim3'
    (_, _, _) -> throw @"compilationError" Types.PrimTypeError
transPrim
  ( Types.BinOp
      Types.Exp
      prim
      CoreErased.Ann
        { CoreErased.term = CoreErased.Prim (Types.FEInteger i)
        }
    )
    | i == 1 =
      transTerm prim
    | otherwise =
      Types.FEInteger (i - 1)
        |> wrap
        |> Types.BinOp Types.Exp prim
        |> wrap
        |> Types.BinOp Types.Mul prim
        |> transPrim
transPrim (Types.BinOp Types.Exp _ _) =
  throw @"compilationError" Types.PrimTypeError

type CircuitExp t =
  Expr.Expr Circuit.Wire Par.F t

type TwoPrimExpression input res m =
  Env.HasCompErr m =>
  (input -> input -> res) ->
  Types.Term ->
  Types.Term ->
  m Types.Expression

onTwoPrim :: TwoPrimExpression Types.Expression (m Types.Expression) m
onTwoPrim f prim1 prim2 = do
  prim1 <- transTerm prim1
  prim2 <- transTerm prim2
  f prim1 prim2

onTwoPrimBool :: TwoPrimExpression (CircuitExp Bool) (CircuitExp Bool) m
onTwoPrimBool f =
  onTwoPrim g
  where
    g (Types.BoolExp prim1) (Types.BoolExp prim2) =
      f prim1 prim2
        |> Types.BoolExp
        |> Env.write
    g _ _ = throw @"compilationError" Types.PrimTypeError

onTwoPrimFGen ::
  (a -> Types.Expression) -> TwoPrimExpression (CircuitExp Par.F) a m
onTwoPrimFGen ret f =
  onTwoPrim g
  where
    g (Types.FExp prim1) (Types.FExp prim2) =
      f prim1 prim2
        |> ret
        |> Env.write
    g _ _ = throw @"compilationError" Types.PrimTypeError

onTwoPrimF :: TwoPrimExpression (CircuitExp Par.F) (CircuitExp Par.F) m
onTwoPrimF = onTwoPrimFGen Types.FExp

onTwoPrimFEq :: TwoPrimExpression (CircuitExp Par.F) (CircuitExp Bool) m
onTwoPrimFEq = onTwoPrimFGen Types.BoolExp

add, mul, sub, eq, and', or', exp :: Types.Term -> Types.Term -> Types.Term
add term term' = wrap (Types.BinOp Types.Add term term')
mul term term' = wrap (Types.BinOp Types.Mul term term')
sub term term' = wrap (Types.BinOp Types.Sub term term')
eq term term' = wrap (Types.BinOp Types.Eq term term')
and' term term' = wrap (Types.BinOp Types.And term term')
or' term term' = wrap (Types.BinOp Types.Or term term')
exp term term' = wrap (Types.BinOp Types.Exp term term')

neg :: Types.Term -> Types.Term
neg = wrap . Types.UnaryOp Types.Neg

c :: Par.F -> Types.Term
c = wrap . Types.Element

int :: Int -> Types.Term
int = wrap . Types.FEInteger

true, false :: Types.Term
true = wrap $ Types.Boolean True
false = wrap $ Types.Boolean False

cond :: Types.Term -> Types.Term -> Types.Term -> Types.Term
cond term term' term'' = wrap (Types.If term term' term'')

wrap :: Types.PrimVal -> Types.Term
wrap prim =
  CoreErased.Ann
    { CoreErased.term = CoreErased.Prim prim,
      CoreErased.usage = Usage.Omega,
      CoreErased.type' = CoreErased.PrimTy ()
    }

var :: Symbol -> Types.Term
var x =
  CoreErased.Ann
    { CoreErased.term = CoreErased.Var x,
      CoreErased.usage = Usage.Omega,
      CoreErased.type' = CoreErased.PrimTy ()
    }

lambda :: [Symbol] -> Types.Term -> Types.Term
lambda args body =
  CoreErased.Ann
    { CoreErased.term =
        CoreErased.LamM
          { CoreErased.arguments = args,
            CoreErased.body = body,
            CoreErased.capture = []
          },
      CoreErased.usage = Usage.Omega,
      CoreErased.type' = CoreErased.Star (toEnum . length $ args)
    }

input :: Int -> Circuit.Expr Circuit.Wire f f
input = Circuit.EVar . Circuit.InputWire
