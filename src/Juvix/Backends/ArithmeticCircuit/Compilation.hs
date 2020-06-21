module Juvix.Backends.ArithmeticCircuit.Compilation
  ( compile
  , add
  , mul
  , sub
  , neg
  , eq
  , exp
  , int
  , c
  , and'
  , or'
  , Term
  , Type
  , lambda
  , var
  , input
  , cond
  , true
  , false
  , runCirc
  ) where

import Juvix.Library hiding (Type, exp)
import Juvix.Backends.ArithmeticCircuit.Compilation.Types
import Juvix.Backends.ArithmeticCircuit.Compilation.Environment
import qualified Circuit
import qualified Circuit.Expr as Expr
import qualified Juvix.Backends.ArithmeticCircuit.Parameterisation as Par
import qualified Circuit.Lang as Lang
import qualified Juvix.Core.ErasedAnn as CoreErased
import qualified Juvix.Core.Usage as Usage
import qualified Data.Map()
import Numeric.Natural()
import qualified Juvix.Library as J

compile :: Term -> Type -> (Either CompilationError ArithExpression, Circuit.ArithCircuit Par.F)
compile term _ = toArithCircuit <$> runState (runExceptT (antiAlias $ transTerm term)) (Env mempty NoExp)
  where
    toArithCircuit env =
      case compilation env of
        BoolExp exp -> runCirc exp
        FExp exp -> runCirc exp
        NoExp -> panic "Tried to compile an empty circuit"

runCirc :: Num f => Expr.Expr Circuit.Wire f ty -> Circuit.ArithCircuit f
runCirc = Expr.execCircuitBuilder . Expr.compile

transTerm :: Term -> ArithmeticCircuitCompilation ArithExpression
transTerm CoreErased.Ann{ CoreErased.term = CoreErased.Prim term } =
  transPrim term

transTerm CoreErased.Ann{ CoreErased.term = CoreErased.Var var } =
  do
    (n, exp) <- lookup var
    case exp of
      NoExp -> do
        _ <- insert var (FExp $ input n)
        write (FExp $ input n)
      _     -> write exp

transTerm CoreErased.Ann{ CoreErased.term = CoreErased.LamM { CoreErased.body = body
                                                            , CoreErased.arguments = arguments}} =
  do
    freshVars arguments
    transTerm body


transTerm CoreErased.Ann{ CoreErased.term = CoreErased.AppM f params } =
  case f of
    (CoreErased.Ann { CoreErased.term = CoreErased.LamM { CoreErased.body =  body
                                                        , CoreErased.arguments = arguments
                                                        }
                    }) ->
      do
        mapM_ execParams (zip arguments params)
        term <- transTerm body

        -- We must remove variables introduced by current function
        mapM_ remove arguments

        return term

     where
       execParams :: (Symbol, Term) -> ArithmeticCircuitCompilation ArithExpression
       execParams (sy, term) =
         do
           term' <- transTerm term
           insert sy term'

    _ -> throw @"compilationError" TypeErrorApplicationNonFunction

transPrim :: PrimVal -> ArithmeticCircuitCompilation ArithExpression
transPrim (Element f) = write . FExp $ Lang.c f
transPrim (Boolean b) = write . BoolExp $ Circuit.EConstBool b
transPrim (FEInteger i) = write . FExp $ Lang.c (fromIntegral i)


-- ArithExpression cannot be made a Functor since the structure inside it is not a Functor
-- It is a Bifunctor where I need to fix the last variant, making it a Contravariant Functor
-- Therefore, we are sticking to manual Applicative until we move away from `arithmetic-circuits`
transPrim (BinOp Add prim prim') = do
  prim1 <- transTerm prim
  prim2 <- transTerm prim'
  case (prim1, prim2) of
    (FExp prim1', FExp prim2') -> (write . FExp) $ Lang.add prim1' prim2'
    (_, _) -> throw @"compilationError" PrimTypeError
transPrim (BinOp Mul prim prim') = do
  prim1 <- transTerm prim
  prim2 <- transTerm prim'
  case (prim1, prim2) of
    (FExp prim1', FExp prim2') -> (write . FExp) $ Lang.mul prim1' prim2'
    (_, _) -> throw @"compilationError" PrimTypeError
transPrim (BinOp Sub prim prim') = do
  prim1 <- transTerm prim
  prim2 <- transTerm prim'
  case (prim1, prim2) of
    (FExp prim1', FExp prim2') -> (write . FExp) $ Lang.sub prim1' prim2'
    (_, _) -> throw @"compilationError" PrimTypeError

-- It implements exponentiation by hand since `arithmetic-circuits` does not support it
transPrim (BinOp Exp prim CoreErased.Ann { CoreErased.term = CoreErased.Prim (FEInteger i)})
    | i == 1 = transTerm prim
    | otherwise = transPrim (BinOp Mul prim (wrap $ BinOp Exp prim (wrap $ FEInteger (i - 1))))

transPrim (BinOp Eq prim prim') = do
  prim1 <- transTerm prim
  prim2 <- transTerm prim'
  case (prim1, prim2) of
    (FExp prim1', FExp prim2') -> (write . BoolExp) $ Lang.eq prim1' prim2'
    (_, _) -> throw @"compilationError" PrimTypeError
transPrim (BinOp And prim prim') = do
  prim1 <- transTerm prim
  prim2 <- transTerm prim'
  case (prim1, prim2) of
    (BoolExp prim1', BoolExp prim2') -> (write . BoolExp) $ Lang.and_ prim1' prim2'
    (_, _) -> throw @"compilationError" PrimTypeError
transPrim (BinOp Or prim prim') = do
  prim1 <- transTerm prim
  prim2 <- transTerm prim'
  case (prim1, prim2) of
    (BoolExp prim1', BoolExp prim2') -> (write . BoolExp) $ Lang.or_ prim1' prim2'
    (_, _) -> throw @"compilationError" PrimTypeError
transPrim (Op Neg prim) = do
  prim1 <- transTerm prim
  case prim1 of
    BoolExp prim1' -> (write . BoolExp) $ Lang.not_ prim1'
    _ -> throw @"compilationError" PrimTypeError
transPrim (If prim prim' prim'') = do
  prim1 <- transTerm prim
  prim2 <- transTerm prim'
  prim3 <- transTerm prim''
  case (prim1, prim2, prim3) of
    (BoolExp prim1', FExp prim2', FExp prim3') -> (write . FExp) $ Lang.cond prim1' prim2' prim3'
    (_, _, _) -> throw @"compilationError" PrimTypeError
transPrim _ = throw @"compilationError" PrimTypeError

add, mul, sub, eq, and', or', exp :: Term -> Term -> Term
add term term' = wrap (BinOp Add term term')
mul term term' = wrap (BinOp Mul term term')
sub term term' = wrap (BinOp Sub term term')
eq term term' = wrap (BinOp Eq term term')
and' term term' = wrap (BinOp And term term')
or' term term' = wrap (BinOp Or term term')
exp term term' = wrap (BinOp Exp term term')

neg :: Term -> Term
neg = wrap . Op Neg

c :: Par.F -> Term
c = wrap . Element

int :: Int -> Term
int = wrap . FEInteger

true, false :: Term
true = wrap $ Boolean True
false = wrap $ Boolean False

cond :: Term -> Term -> Term -> Term
cond term term' term'' = wrap (If term term' term'')

wrap :: PrimVal -> Term
wrap prim = CoreErased.Ann { CoreErased.term = CoreErased.Prim prim
                           , CoreErased.usage = Usage.Omega
                           , CoreErased.type' = CoreErased.PrimTy ()
                           }

var :: J.Symbol -> Term
var x = CoreErased.Ann { CoreErased.term = CoreErased.Var x
                       , CoreErased.usage = Usage.Omega
                       , CoreErased.type' = CoreErased.PrimTy ()
                       }

lambda :: [J.Symbol] -> Term -> Term
lambda args body = CoreErased.Ann { CoreErased.term = CoreErased.LamM { CoreErased.arguments = args
                                                                      , CoreErased.body = body
                                                                      , CoreErased.capture = [] }
                                    , CoreErased.usage = Usage.Omega
                                    , CoreErased.type' = CoreErased.Star (toEnum . length $ args)
                                    }

input :: Int -> Circuit.Expr Circuit.Wire f f
input = Circuit.EVar . Circuit.InputWire
