module Juvix.Core.EAC.ConstraintGen where

import Control.Arrow (left)
import Juvix.Core.EAC.Types
import Juvix.Core.Erased.Types
import Juvix.Library hiding (Type, link, reduce)
import qualified Juvix.Library.HashMap as Map
import Prelude (error)

-- TODO include actual results for RPRIM

{- Main functionality. -}

-- Construct occurrence map.
setOccurrenceMap ∷ ∀ m primVal. (HasState "occurrenceMap" OccurrenceMap m) ⇒ Term primVal → m ()
setOccurrenceMap term = do
  case term of
    Var sym →
      modify' @"occurrenceMap" (Map.insertWith (+) sym 1)
    Lam _ b → do
      setOccurrenceMap b
    App a b → do
      setOccurrenceMap a
      setOccurrenceMap b

-- Parameterize type.
parameterizeType ∷
  ∀ m primTy.
  ( HasState "nextParam" Param m,
    HasWriter "constraints" [Constraint] m
  ) ⇒
  Type primTy →
  m (PType primTy)
parameterizeType ty = do
  param ← freshParam
  -- Typing constraint: m >= 0
  addConstraint (Constraint [ConstraintVar 1 param] (Gte 0))
  case ty of
    SymT sym →
      pure (PSymT param sym)
    Pi arg body → do
      arg ← parameterizeType arg
      body ← parameterizeType body
      pure (PArrT param arg body)

-- Parameterize type assignment.
parameterizeTypeAssignment ∷
  ∀ m primTy.
  ( HasState "nextParam" Param m,
    HasWriter "constraints" [Constraint] m,
    HasState "typeAssignment" (TypeAssignment primTy) m
  ) ⇒
  m (ParamTypeAssignment primTy)
parameterizeTypeAssignment = do
  assignment ← get @"typeAssignment"
  traverse parameterizeType assignment

-- Reparameterize.
reparameterize ∷
  ∀ m primTy.
  ( HasState "nextParam" Param m,
    HasWriter "constraints" [Constraint] m
  ) ⇒
  PType primTy →
  m (PType primTy)
reparameterize pty = do
  param ← freshParam
  -- Typing constraint: m >= 0
  addConstraint (Constraint [ConstraintVar 1 param] (Gte 0))
  case pty of
    PSymT _ sym → pure (PSymT param sym)
    PArrT _ a b → pure (PArrT param a b)

unificationConstraints ∷
  ∀ m primTy.
  (HasWriter "constraints" [Constraint] m) ⇒
  PType primTy →
  PType primTy →
  m ()
unificationConstraints (PSymT a _) (PSymT b _) =
  addConstraint (Constraint [ConstraintVar 1 a, ConstraintVar (- 1) b] (Eq 0))
unificationConstraints (PArrT a aArg aRes) (PArrT b bArg bRes) = do
  addConstraint (Constraint [ConstraintVar 1 a, ConstraintVar (- 1) b] (Eq 0))
  unificationConstraints aArg bArg
  unificationConstraints aRes bRes

--unificationConstraints x y = error ("cannot unify " <> show x <> " with " <> show y)

-- Generate boxing & typing constraints.
-- In one pass to avoid keeping extra maps.
-- TODO ∷ Change occurrenceMap to a reader at this point in the code
boxAndTypeConstraint ∷
  ∀ m primTy primVal.
  ( HasState "path" Path m,
    HasState "varPaths" VarPaths m,
    HasState "nextParam" Param m,
    HasWriter "constraints" [Constraint] m,
    HasState "occurrenceMap" OccurrenceMap m
  ) ⇒
  ParamTypeAssignment primTy →
  Term primVal →
  m (RPT primVal, PType primTy)
boxAndTypeConstraint parameterizedAssignment term = do
  let rec = boxAndTypeConstraint parameterizedAssignment
  varPaths ← get @"varPaths"
  param ← addPath
  path ← get @"path"
  -- Boxing constraint.
  addConstraint (Constraint (ConstraintVar 1 <$> path) (Gte 0))
  case term of
    Var sym → do
      -- Boxing constraint.
      case varPaths Map.!? sym of
        Just loc → addConstraint (Constraint (ConstraintVar 1 <$> dropWhile (< loc) path) (Eq 0))
        Nothing → addConstraint (Constraint (ConstraintVar 1 <$> path) (Eq 0))
      let origParamTy = parameterizedAssignment Map.! sym
      -- Typing constraint: variable occurrences.
      occurrenceMap ← get @"occurrenceMap"
      let occurrences = occurrenceMap Map.! sym
          origBangParam = bangParam origParamTy
      -- If non-linear (>= 2 occurrences), original type must be of form !A.
      when (occurrences >= 2) $
        addConstraint (Constraint [ConstraintVar 1 origBangParam] (Gte 1))
      -- Calculate parameterized type for subterm.
      paramTy ← reparameterize origParamTy
      -- Typing constraint: m = k + n ⟹ k + n - m = 0
      -- where n = param
      -- and   k = origBangParam
      -- and   m = bangParam paramTy
      addConstraint
        ( Constraint
            [ ConstraintVar (- 1) (bangParam paramTy),
              ConstraintVar 1 origBangParam,
              ConstraintVar 1 param
            ]
            (Eq 0)
        )
      -- Typing constraint: m >= 0
      addConstraint (Constraint [ConstraintVar 1 (bangParam paramTy)] (Gte 0))
      -- Return parameterized term.
      pure (RBang param (RVar sym), paramTy)
    Lam sym body → do
      nextParam ← getNextParam
      modify' @"varPaths" (Map.insert sym nextParam)
      (body, bodyTy) ← rec body
      -- Calculate parameterized type for subterm.
      lamTyParam ← freshParam
      -- Typing constraint: m >= 0
      addConstraint (Constraint [ConstraintVar 1 lamTyParam] (Gte 0))
      let argTy = parameterizedAssignment Map.! sym
          lamTy = PArrT lamTyParam argTy bodyTy
      -- Calculate final type.
      resTy ← reparameterize lamTy
      -- Typing constraint: m = 0
      addConstraint (Constraint [ConstraintVar 1 lamTyParam] (Eq 0))
      -- Typing constraint: m = k + n ⟹ k + n - m = 0
      -- where k = bangParam lamTy ∈ ℤ
      -- and   m = bangParam resTy ∈ ℤ
      -- and   n = param           ∈ ℤ
      addConstraint
        ( Constraint
            [ ConstraintVar (- 1) (bangParam resTy),
              ConstraintVar 1 (bangParam lamTy),
              ConstraintVar 1 param
            ]
            (Eq 0)
        )
      -- Typing constraint: m >= 0
      addConstraint (Constraint [ConstraintVar 1 (bangParam resTy)] (Gte 0))
      -- Return parameterized term.
      pure (RBang param (RLam sym body), resTy)
    App a b → do
      (a, aTy) ← rec a
      let PArrT bangA argTy resTy = aTy
      put @"path" path
      put @"varPaths" varPaths
      (b, bTy) ← rec b
      -- Calculate parameterized type for subterm.
      appTy ← reparameterize resTy
      -- Typing constraint: U(A₁, A₂) ∪ m = 0
      -- where the terms are from:
      -- aTy = !^m(A₁ ⊸ B₁)
      -- bTy = A₂
      unificationConstraints argTy bTy
      addConstraint (Constraint [ConstraintVar 1 bangA] (Eq 0))
      -- Typing constraint: m = k + n ⟹ k + n - m = 0
      -- where n = param
      -- and   k = bangParam resTy
      -- and   m = bangParam appTy
      addConstraint
        ( Constraint
            [ ConstraintVar (- 1) (bangParam appTy),
              ConstraintVar 1 (bangParam resTy),
              ConstraintVar 1 param
            ]
            (Eq 0)
        )
      -- Typing constraint: m >= 0
      addConstraint (Constraint [ConstraintVar 1 (bangParam appTy)] (Gte 0))
      -- Return parameterized term.
      pure (RBang param (RApp a b), appTy)

-- Generate constraints.
generateTypeAndConstraints ∷
  ∀ m primTy primVal.
  ( HasState "path" Path m,
    HasState "varPaths" VarPaths m,
    HasState "nextParam" Param m,
    HasState "typeAssignment" (TypeAssignment primTy) m,
    HasWriter "constraints" [Constraint] m,
    HasState "occurrenceMap" OccurrenceMap m
  ) ⇒
  Term primVal →
  m (RPT primVal, ParamTypeAssignment primTy)
generateTypeAndConstraints term = do
  parameterizedAssignment ← parameterizeTypeAssignment
  setOccurrenceMap term
  boxAndTypeConstraint parameterizedAssignment term
    >>| second (const parameterizedAssignment)

generateConstraints ∷
  ( HasState "path" Path m,
    HasState "varPaths" VarPaths m,
    HasState "nextParam" Param m,
    HasState "typeAssignment" (TypeAssignment primTy) m,
    HasWriter "constraints" [Constraint] m,
    HasState "occurrenceMap" OccurrenceMap m
  ) ⇒
  Term primVal →
  m (RPT primVal)
generateConstraints term =
  generateTypeAndConstraints term
    >>| fst

{- Bracket Checker. -}
bracketChecker ∷ RPTO primVal → Either BracketErrors ()
bracketChecker t = runEither (rec' t 0 mempty)
  where
    rec' (RBang changeBy (RVar sym)) n map =
      let f x
            | changeBy + n + x == 0 = pure ()
            | changeBy + n + x > 0 = throw @"typ" TooManyOpenV
            | otherwise = throw @"typ" TooManyClosingV
       in case map Map.!? sym of
            Just x → f x
            Nothing → f 0
    rec' (RBang changeBy t) n map
      | changeBy + n < 0 = throw @"typ" TooManyClosing
      | otherwise =
        let n' = changeBy + n
         in case t of
              RLam s t → rec' t n' (Map.insert s (negate n') map)
              RApp t1 t2 → rec' t1 n' map >> rec' t2 n' map
              RVar _ → error "already is matched"
              RPrim _ → undefined

bracketCheckerErr ∷ RPTO primVal → Either (Errors primTy primVal) ()
bracketCheckerErr t = left Brack (bracketChecker t)

{- Type Checker. -}
-- Precondition ∷ all terms inside of RPTO must be unique
typChecker ∷ ∀ primTy primVal. (Eq primTy) ⇒ RPTO primVal → ParamTypeAssignment primTy → Either (TypeErrors primTy primVal) ()
typChecker t typAssign = runEither (() <$ rec' t typAssign)
  where
    rec' (RBang bangVar (RVar s)) assign =
      case assign Map.!? s of
        Nothing → throw @"typ" MissingOverUse
        Just t → do
          newTyp ← addParamPos bangVar t
          if
            | bangParam t > 0 → pure (assign, newTyp)
            | otherwise → pure (Map.delete s assign, newTyp)
    rec' (RBang bangApp term@(RApp t1 t2)) assign = do
      (newAssign, type1) ← rec' t1 assign
      (newAssign', type2) ← rec' t2 newAssign
      case type1 of
        PArrT _ arg result
          | arg == type2 → do
            newTyp ← addParamPos bangApp result
            pure (newAssign', newTyp)
          | otherwise → throw @"typ" (MisMatchArguments arg type2 term)
        t@PSymT {} → throw @"typ" (TypeIsNotFunction t)
    rec' (RBang x (RLam s t)) assign = do
      (newAssign, bodyType) ← rec' t assign
      case assign Map.!? s of
        Just arg → pure (newAssign, PArrT x arg bodyType)
        Nothing → throw @"typ" MissingOverUse
    rec' (RBang _bangVar (RPrim _p)) _assign = undefined

typCheckerErr ∷ ∀ primTy primVal. (Eq primTy) ⇒ RPTO primVal → ParamTypeAssignment primTy → Either (Errors primTy primVal) ()
typCheckerErr t typeAssing = left Typ (typChecker t typeAssing)

{- Utility. -}

-- The outer bang parameter.
bangParam ∷ ∀ primTy. PType primTy → Param
bangParam (PSymT param _) = param
bangParam (PArrT param _ _) = param

putParam ∷ ∀ primTy. Param → PType primTy → PType primTy
putParam p (PSymT _ s) = PSymT p s
putParam p (PArrT _ t1 t2) = PArrT p t1 t2

-- putParamPos ∷ Param → PType → PType
addParamPos ∷ ∀ m primTy primVal. HasThrow "typ" (TypeErrors primTy primVal) m ⇒ Param → PType primTy → m (PType primTy)
addParamPos toAdd (PSymT p s)
  | toAdd + p < 0 = throw @"typ" TooManyHats
  | otherwise = pure (PSymT (toAdd + p) s)
addParamPos toAdd (PArrT p t1 t2)
  | toAdd + p < 0 = throw @"typ" TooManyHats
  | otherwise = pure (PArrT (toAdd + p) t1 t2)

-- | Get the next fresh parameter
getNextParam ∷ ∀ b f. (HasState "nextParam" b f, Enum b) ⇒ f b
getNextParam = get @"nextParam"

-- | Generate fresh parameter.
freshParam ∷ ∀ m. (HasState "nextParam" Param m) ⇒ m Param
freshParam = do
  param ← get @"nextParam"
  put @"nextParam" (succ param)
  pure param

-- Append to path.
addPath ∷ ∀ m. (HasState "nextParam" Param m, HasState "path" Path m) ⇒ m Param
addPath = do
  param ← freshParam
  modify' @"path" (<> [param])
  pure param

-- Add constraint.
addConstraint ∷ ∀ m. HasWriter "constraints" [Constraint] m ⇒ Constraint → m ()
addConstraint con = tell @"constraints" [con]

-- Execute with prior assignment.
execWithAssignment ∷ ∀ primTy a. TypeAssignment primTy → EnvConstraint primTy a → (a, Env primTy)
execWithAssignment assignment (EnvCon env) =
  runState env (Env [] mempty assignment 0 [] mempty)
