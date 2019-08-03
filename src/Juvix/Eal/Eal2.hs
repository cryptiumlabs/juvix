module Juvix.Eal.Eal2 where

import qualified          Data.Map.Strict  as Map
import           Juvix.Eal.Types2
import           Juvix.Library    hiding (link, reduce, Type)

{- Main functionality. -}

-- Parameterize type.
parameterizeType :: HasState "nextParam" Param m
  => Type -> m PType
parameterizeType ty = do
  param <- freshParam
  case ty of
    SymT sym ->
      pure (PSymT param sym)
    ArrT arg body -> do
      arg <- parameterizeType arg
      body <- parameterizeType body
      pure (PArrT param arg body)

-- Parameterize type assignment.
parameterizeTypeAssignment :: (HasState "nextParam" Param m,
                               HasState "typeAssignment" TypeAssignment m)
 => m ParamTypeAssignment
parameterizeTypeAssignment = do
  assignment <- get @"typeAssignment"
  mapM parameterizeType assignment

-- Reparameterize.
reparameterize :: (HasState "nextParam" Param m)
 => PType -> m PType
reparameterize pty = do
  param <- freshParam
  case pty of
    PSymT _ sym -> pure (PSymT param sym)
    PArrT _ a b -> pure (PArrT param a b)

-- Generate boxing & typing constraints.
-- In one pass to avoid keeping extra maps.
boxAndTypeConstraint :: (HasState "path" Path m,
                         HasState "varPaths" VarPaths m,
                         HasState "nextParam" Param m,
                         HasState "constraints" [Constraint] m)
 => ParamTypeAssignment -> Term -> m (RPT, PType)
boxAndTypeConstraint parameterizedAssignment term = do
  let rec = boxAndTypeConstraint parameterizedAssignment
  varPaths ← get @"varPaths"
  param <- addPath
  path  <- get @"path"
  addConstraint (Constraint (map (ConstraintVar 1) path) (Gte 0))
  case term of
    Var sym -> do
      -- Boxing constraint.
      case varPaths Map.!? sym of
        Just loc  → addConstraint (Constraint (map (ConstraintVar 1) (dropWhile (< loc) path)) (Eq 0))
        Nothing   → addConstraint (Constraint (map (ConstraintVar 1) path) (Eq 0))
      -- Calculate parameterized type for subterm.
      let paramTy = parameterizedAssignment Map.! sym
      paramTy <- reparameterize paramTy
      -- Typing constraint.
      -- TODO.
      pure (RBang param (RVar sym), paramTy)
    Lam sym body -> do
      modify' @"varPaths" (Map.insert sym (succ param))
      (body, bodyTy) ← rec body
      -- Calculate parameterized type for subterm.
      param <- freshParam
      let argTy = parameterizedAssignment Map.! sym
          lamTy = PArrT param argTy bodyTy
      -- Typing constraint.
      -- TODO.
      pure (RBang param (RLam sym body), lamTy)
    App a b -> do
      (a, aTy) <- rec a
      put @"path" path
      put @"varPaths" varPaths
      (b, bTy) <- rec b
      -- Calculate parameterized type for subterm.
      let PArrT _ _ resTy = aTy
          appTy = resTy
      -- Typing constraint.
      -- TODO.
      pure (RBang param (RApp a b), appTy)

-- Generate constraints.
generateConstraints :: (HasState "path" Path m,
                        HasState "varPaths" VarPaths m,
                        HasState "nextParam" Param m,
                        HasState "typeAssignment" TypeAssignment m,
                        HasState "constraints" [Constraint] m)
 => Term -> m RPT
generateConstraints term = do
  parameterizedAssignment <- parameterizeTypeAssignment
  fst |<< boxAndTypeConstraint parameterizedAssignment term

{- Utility. -}

-- Generate fresh parameter.
freshParam :: (HasState "nextParam" Param m) => m Param
freshParam = do
  param <- get @"nextParam"
  put  @"nextParam" (succ param)
  pure param

-- Append to path.
addPath :: (HasState "nextParam" Param m, HasState "path" Path m) ⇒ m Param
addPath = do
  param ← freshParam
  modify' @"path" (<> [param])
  pure param

-- Add constraint.
addConstraint :: HasState "constraints" [Constraint] m ⇒ Constraint → m ()
addConstraint con = modify' @"constraints" (con :)

-- Execute with prior assignment.
execWithAssignment :: TypeAssignment -> EnvConstraint a -> (a, Env)
execWithAssignment assignment (EnvCon env) = runState env (Env [] mempty assignment 0 [])

-- Test term: \s . \z . s s z.
testTerm :: Term
testTerm = Lam (someSymbolVal "s") (Lam (someSymbolVal "z") (App (Var (someSymbolVal "s")) (App (Var (someSymbolVal "s")) (Var (someSymbolVal "z")))))

-- Test assignment - s : a -> a, z : a.
testAssignment :: TypeAssignment
testAssignment = Map.fromList [
  (someSymbolVal "s", ArrT (SymT (someSymbolVal "a")) (SymT (someSymbolVal "a"))),
  (someSymbolVal "z", SymT (someSymbolVal "a"))
  ]
