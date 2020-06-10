module Juvix.Backends.ArithmeticCircuit.Compilation.Types where

import qualified Juvix.Core.ErasedAnn as CoreErased
import qualified Juvix.Core.Usage as Usage
import qualified Juvix.Library as J
import qualified Juvix.Backends.ArithmeticCircuit.Parameterisation as Par
import qualified Circuit.Expr as Expr
import qualified Circuit.Lang as Lang
import qualified Circuit
import qualified Data.Map as Map
import Numeric.Natural()

import Protolude

data PrimVal
  = Element Par.F
  | Boolean Bool
  | FEInteger Int
  | BinOp BinOp Term Term
  | Op Op Term
  | If Term Term Term
  deriving (Show, Eq)

data BinOp
  = Add
  | Mul
  | Sub
  | Exp
  | Eq
  | And
  | Or
  deriving (Show, Eq)

data Op = Neg
  deriving (Show, Eq)

type Term = CoreErased.AnnTerm () PrimVal
type Type = CoreErased.Type () PrimVal

data CompilationError
  = NotYetImplemented
  | SomethingWentWrongSorry
  | VariableOutOfScope
  deriving (Eq, Show)

data ArithExpression f
  = BoolExp (Expr.Expr Circuit.Wire f Bool)
  | FExp (Expr.Expr Circuit.Wire f f)
  | NoExp

data Memory = Mem (Map.Map J.Symbol (Int, ArithExpression Par.F)) Int

instance Semigroup Memory where
  (Mem map_ n) <> (Mem map__ m) = Mem (map_ <> Map.map (shift n) map__) (n + m)
    where shift n (m, exp) = (n + m, exp)

instance Monoid Memory where
  mempty = Mem Map.empty 0
  mappend = (<>)

insert :: J.Symbol -> ArithExpression Par.F -> Memory -> Memory
insert sy x (Mem map_ n) = Mem (Map.insert sy (n, x) map_) (n + 1)

lookup :: J.Symbol -> Memory -> Maybe (Int, ArithExpression Par.F)
lookup sy (Mem map_ _) =
  case Map.lookup sy map_ of
    Just res -> Just res
    Nothing -> Nothing

transTerm :: Memory
     -> Term
     -> (Memory, ArithExpression Par.F)
transTerm mem CoreErased.Ann{ CoreErased.term = CoreErased.Prim term } =
  (mem, transPrim term mem)
transTerm mem CoreErased.Ann{ CoreErased.term = CoreErased.Var var } =
  case lookup var mem of
    Just (n, NoExp) -> (mem, FExp $ input n)
    Just (_, res) -> (mem, res)
    Nothing -> panic $ show VariableOutOfScope
transTerm mem CoreErased.Ann{ CoreErased.term = CoreErased.LamM{ CoreErased.body = body
                                                          , CoreErased.arguments = arguments}} =
  let mem' = freshVars mem arguments
  in transTerm mem' body
transTerm mem CoreErased.Ann{ CoreErased.term = CoreErased.AppM f params } =
  case f of
    CoreErased.Ann { CoreErased.term = CoreErased.LamM { CoreErased.body =  body
                                                       , CoreErased.arguments = arguments
                                                       }
                   } -> let transTermParams = snd . sequenceA $ map (transTerm mem) params
                            matchedArgParams = zip arguments transTermParams
                            mem' = foldr (\(sy, val) m -> insert sy val m) mem matchedArgParams
                         in transTerm mem' body
    _ -> panic $ show SomethingWentWrongSorry

transPrim :: PrimVal -> Memory -> ArithExpression Par.F
transPrim (Element f) _ = FExp $ Lang.c f
transPrim (Boolean b) _ = BoolExp $ Circuit.EConstBool b
transPrim (FEInteger i) _ = FExp $ Lang.c (fromIntegral i)
-- I miss lenses
transPrim (BinOp Add prim prim') mem = let (_, FExp prim1) = transTerm mem prim
                                           (_, FExp prim2) = transTerm mem prim'
                                           in FExp $ Lang.add prim1 prim2
transPrim (BinOp Mul prim prim') mem = let (_, FExp prim1) = transTerm mem prim
                                           (_, FExp prim2) = transTerm mem prim'
                                           in FExp $ Lang.mul prim1 prim2
transPrim (BinOp Sub prim prim') mem = let (_, FExp prim1) = transTerm mem prim
                                           (_, FExp prim2) = transTerm mem prim'
                                           in FExp $ Lang.sub prim1 prim2
transPrim (BinOp Eq prim prim') mem = let (_, FExp prim1) = transTerm mem prim
                                          (_, FExp prim2) = transTerm mem prim'
                                          in BoolExp $ Lang.eq prim1 prim2
transPrim (BinOp And prim prim') mem = let (_, BoolExp prim1) = transTerm mem prim
                                           (_, BoolExp prim2) = transTerm mem prim'
                                           in BoolExp $ Lang.and_ prim1 prim2
transPrim (BinOp Or prim prim') mem = let (_, BoolExp prim1) = transTerm mem prim
                                          (_, BoolExp prim2) = transTerm mem prim'
                                          in BoolExp $ Lang.or_ prim1 prim2
-- implements exponentiation by hand
transPrim (BinOp Exp prim CoreErased.Ann { CoreErased.term = CoreErased.Prim (FEInteger i)}) mem
    | i == 1 = snd $ transTerm mem prim
    | otherwise = transPrim (BinOp Mul prim (wrap $ BinOp Exp prim (wrap $ FEInteger (i - 1)))) mem
transPrim (Op Neg prim) mem = let (_, BoolExp prim') = transTerm mem prim
                                  in BoolExp $ Lang.not_ prim'
transPrim (If prim prim' prim'') mem = let (_, BoolExp prim1) = transTerm mem prim
                                           (_, FExp prim2) = transTerm mem prim'
                                           (_, FExp prim3) = transTerm mem prim''
                                  in FExp $ Lang.cond prim1 prim2 prim3

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

extractPrim :: Term -> PrimVal
extractPrim CoreErased.Ann { CoreErased.term = CoreErased.Prim prim } = prim

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

freshVars :: Memory -> [J.Symbol] -> Memory
freshVars (Mem map_ n) vars = Mem (map_ <> Map.fromList (zip vars (slots n))) (n + 1)
  where slots :: Int -> [(Int, ArithExpression Par.F)]
        slots n = map (, NoExp) [n ..]
