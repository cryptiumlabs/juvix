module Juvix.EAL.Solve where

import qualified Data.Map        as Map
import qualified Data.Set        as Set
import           GHC.Base        (String)
import qualified Z3.Monad        as Z3

import           Juvix.EAL.Types
import           Juvix.Library   hiding (link, reduce)


runMultipleConstraints ∷ Int → [Constraint] → RPT → IO ()
runMultipleConstraints numRepeat constraints syntax = do
  let numset = grabTermNumbers syntax mempty
      recGen _               _         _      0   = pure ()
      recGen constraintCall printModel consZ3 num = do
        (r, v, s) ← Z3.evalZ3 constraintCall
        printModel v
        putText "-->"
        print (r, s)
        case s of
          Just x →
            let bounds  = filter (\(i,_) → Set.member i numset) (zip [0..] x)
                newCons =
                  bounds
                  >>| (\(i,x) → Constraint [ConstraintVar 1 i]
                                           (Eq (fromInteger x)))
                extraConstraint = makeVarMap newCons
                                  >>= flip constraintsToZ3 newCons
                                  >>= Z3.mkNot
            in
            recNext extraConstraint
                    consZ3
                    (pred num)
          Nothing → pure ()

      recFirst             = recGen (constraintSystem constraints)
                                    putStrLn
                                    (pure [])
      recNext extra consZ3 = recGen (do
                                        e   ← extra
                                        z3s ← consZ3
                                        constraintSystemAnd constraints (e : z3s))
                                    (\_ → pure ())
                                    ((:) <$> extra <*> consZ3)
  recFirst numRepeat
  where
    -- Could use a list, since this should be ascending, but I do not assume that
    grabTermNumbers (RBang i (RLam _ t)) s = grabTermNumbers t (Set.insert i s)
    grabTermNumbers (RBang i (RVar _))   s = Set.insert i s
    grabTermNumbers (RBang i (RApp t1 t2)) s =
      grabTermNumbers t1 (grabTermNumbers t2 (Set.insert i s))

getConstraints ∷ [Constraint] → IO (Maybe [Integer])
getConstraints constraints = do
  (r,v,s) ← Z3.evalZ3 (constraintSystem constraints)
  putStrLn v
  putText "-->"
  print (r, s)
  pure s

runConstraints ∷ [Constraint] → IO ()
runConstraints constraints = do
  (r, v, s)  ← Z3.evalZ3 (constraintSystem constraints)
  putStrLn v
  putText "-->"
  print (r, s)

collectVars ∷ [Constraint] → Set.Set Int
collectVars = Set.unions . map Set.singleton . concatMap (map variable . vars)

opToZ3 ∷ Op → Z3.AST → Z3.Z3 Z3.AST
opToZ3 (Eq n) vs = do
  i ← Z3.mkInteger (fromIntegral n)
  Z3.mkEq vs i
opToZ3 (Gte n) vs = do
  i ← Z3.mkInteger (fromIntegral n)
  Z3.mkGe vs i

varToZ3 ∷ Map.Map Int Z3.AST → ConstraintVar → Z3.Z3 Z3.AST
varToZ3 varMap (ConstraintVar coeff var) = do
  let z3Var = varMap Map.! var
  coeff ← Z3.mkInteger (fromIntegral coeff)
  Z3.mkMul [coeff, z3Var]

varsToZ3 ∷ Map.Map Int Z3.AST → [ConstraintVar] → Z3.Z3 Z3.AST
varsToZ3 varMap vars = do
  vars ← traverse (varToZ3 varMap) vars
  case vars of
    [v] → pure v
    _   → Z3.mkAdd vars

constraintToZ3 ∷ Map.Map Int Z3.AST → Constraint → Z3.Z3 Z3.AST
constraintToZ3 varMap (Constraint vars op) = opToZ3 op =<< varsToZ3 varMap vars

constraintsToZ3 ∷ Map.Map Int Z3.AST → [Constraint] → Z3.Z3 Z3.AST
constraintsToZ3 varMap constraints = Z3.mkAnd =<< traverse (constraintToZ3 varMap) constraints

constraintsToZ3Extra ∷ [Z3.AST] → Map Int Z3.AST → [Constraint] → Z3.Z3 Z3.AST
constraintsToZ3Extra extra varMap constraints = do
  cons ← traverse (constraintToZ3 varMap) constraints
  and  ← Z3.mkAnd cons
  Z3.mkAnd (and : extra)

makeVarMap ∷ Z3.MonadZ3 f ⇒ [Constraint] → f (Map Int Z3.AST)
makeVarMap constraints =
  let vars = Set.toList (collectVars constraints) in
  traverse (\v → (Z3.mkStringSymbol ("m_" <> show v)
                 >>= Z3.mkIntVar)
                 >>| (,) v)
           vars
  >>| Map.fromList

constraintSystemGen ∷ Z3.MonadZ3 m
                    ⇒ [Constraint]
                    → (Map Int Z3.AST → [Constraint] → m Z3.AST)
                    → m (Z3.Result, String, Maybe [Integer])
constraintSystemGen constraints modifyAst = do
  varMap ← makeVarMap constraints
  ast    ← modifyAst varMap constraints
  model  ← Z3.astToString ast
  Z3.assert ast
  (res, sol) ← Z3.withModel $ \m →
    catMaybes <$> traverse (Z3.evalInt m) (Map.elems varMap)
  pure (res, model, sol)

constraintSystemAnd ∷ [Constraint] → [Z3.AST] → Z3.Z3 (Z3.Result, GHC.Base.String, Maybe [Integer])
constraintSystemAnd c z3Constraint = constraintSystemGen c (constraintsToZ3Extra z3Constraint)

constraintSystem ∷ [Constraint] → Z3.Z3 (Z3.Result, GHC.Base.String, Maybe [Integer])
constraintSystem constraints = constraintSystemGen constraints constraintsToZ3

-- wtf, this doesn't work
-- it does now!
computeTwo ∷ IO (Maybe Integer)
computeTwo = Z3.evalZ3 $ do
  x  ← Z3.mkIntVar =<< (Z3.mkStringSymbol "x")
  _2 ← Z3.mkInteger 2
  Z3.assert =<< Z3.mkEq x _2
  fmap snd
    $ Z3.withModel
    $ \m -> (\(Just x) -> x) <$> Z3.evalInt m x
