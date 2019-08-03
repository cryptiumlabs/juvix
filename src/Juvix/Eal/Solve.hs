module Juvix.Eal.Solve where

import qualified Data.Map         as Map
import qualified Data.Set         as Set
import           GHC.Base         (String)
import qualified Z3.Monad         as Z3

import           Juvix.Eal.Types2
import           Juvix.Library    hiding (link, reduce)

runConstraints ∷ [Constraint] → IO ()
runConstraints constraints = do
  (r, v, s) <- Z3.evalZ3 (constraintSystem constraints)
  putStrLn v
  putText "-->"
  print (r, s)

collectVars ∷ [Constraint] → Set.Set Int
collectVars = Set.unions . map Set.singleton . concatMap (map variable . vars)

opToZ3 ∷ Op → Z3.AST → Z3.Z3 Z3.AST
opToZ3 (Eq n) vs = do
  Z3.mkEq vs =<< Z3.mkInteger (fromIntegral n)
opToZ3 (Gte n) vs = do
  Z3.mkGe vs =<< Z3.mkInteger (fromIntegral n)

varToZ3 ∷ Map.Map Int Z3.AST → ConstraintVar → Z3.Z3 Z3.AST
varToZ3 varMap (ConstraintVar coeff var) = do
  let z3Var = varMap Map.! var
  coeff <- Z3.mkInteger (fromIntegral coeff)
  Z3.mkMul [coeff, z3Var]

varsToZ3 ∷ Map.Map Int Z3.AST → [ConstraintVar] → Z3.Z3 Z3.AST
varsToZ3 varMap vars = do
  vars <- mapM (varToZ3 varMap) vars
  case vars of
    [v] -> pure v
    _   -> Z3.mkAdd vars

constraintToZ3 ∷ Map.Map Int Z3.AST → Constraint → Z3.Z3 Z3.AST
constraintToZ3 varMap (Constraint vars op) = opToZ3 op =<< varsToZ3 varMap vars

constraintsToZ3 ∷ Map.Map Int Z3.AST → [Constraint] → Z3.Z3 Z3.AST
constraintsToZ3 varMap constraints = Z3.mkAnd =<< mapM (constraintToZ3 varMap) constraints

constraintSystem ∷ [Constraint] → Z3.Z3 (Z3.Result, GHC.Base.String, Maybe [Integer])
constraintSystem constraints = do
  let vars = Set.toList (collectVars constraints)
  varMap <- Map.fromList |<< mapM (\v -> Z3.mkFreshIntVar ("m_" <> show v) >>| (,) v) vars
  ast <- constraintsToZ3 varMap constraints
  model <- Z3.astToString ast
  Z3.assert ast
  (res, sol) <- Z3.withModel $ \m ->
    catMaybes <$> mapM (Z3.evalInt m) (Map.elems varMap)
  pure (res, model, sol)

-- wtf, this doesn't work
computeTwo = Z3.evalZ3 $ do
  x <- Z3.mkFreshIntVar "x"
  _2 <- Z3.mkInteger 2
  Z3.assert =<< Z3.mkEq x _2
  fmap snd $ Z3.withModel $ \m -> (\(Just x) -> x) <$> Z3.evalInt m x
