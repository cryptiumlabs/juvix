module Juvix.Core.EAC.ConstraintGen where

import Control.Arrow (left)
import qualified Juvix.Core.EAC.Types as EAC
import qualified Juvix.Core.Erased.Types as Erased
import qualified Juvix.Core.Types as Core
import Juvix.Library hiding (Type, link, reduce)
import qualified Juvix.Library.HashMap as Map
import Prelude (error)

-- TODO include actual results for RPRIM

{- Main functionality. -}

-- Construct occurrence map.
setOccurrenceMap ::
  (HasState "occurrenceMap" EAC.OccurrenceMap m) => Erased.Term primVal -> m ()
setOccurrenceMap term = do
  case term of
    Erased.Prim _ ->
      pure ()
    Erased.Var sym ->
      modify' @"occurrenceMap" (Map.insertWith (+) sym 1)
    Erased.Lam _ b -> do
      setOccurrenceMap b
    Erased.Pair a b -> do
      setOccurrenceMap a
      setOccurrenceMap b
    Erased.Unit ->
      pure ()
    Erased.Let _ b t -> do
      setOccurrenceMap b
      setOccurrenceMap t
    Erased.App a b -> do
      setOccurrenceMap a
      setOccurrenceMap b

-- Parameterize type.
parameterizeType ::
  forall m primTy.
  ( HasState "nextParam" EAC.Param m,
    HasWriter "constraints" [EAC.Constraint] m
  ) =>
  Erased.Type primTy ->
  m (EAC.PType primTy)
parameterizeType ty = do
  param <- freshParam
  -- Typing constraint: m >= 0
  addConstraint (EAC.Constraint [EAC.ConstraintVar 1 param] (EAC.Gte 0))
  case ty of
    Erased.PrimTy p ->
      pure (EAC.PPrimT p)
    Erased.SymT sym ->
      pure (EAC.PSymT param sym)
    Erased.Pi _ arg body -> do
      arg <- parameterizeType arg
      body <- parameterizeType body
      pure (EAC.PArrT param arg body)
    Erased.Sig {} -> undefined
    Erased.UnitTy -> undefined
    Erased.Star _ -> undefined

-- Parameterize type assignment.
parameterizeTypeAssignment ::
  forall m primTy.
  ( HasState "nextParam" EAC.Param m,
    HasWriter "constraints" [EAC.Constraint] m,
    HasState "typeAssignment" (Erased.TypeAssignment primTy) m
  ) =>
  m (EAC.ParamTypeAssignment primTy)
parameterizeTypeAssignment =
  get @"typeAssignment" >>= traverse parameterizeType

-- Reparameterize.
reparameterize ::
  forall m primTy.
  ( HasState "nextParam" EAC.Param m,
    HasWriter "constraints" [EAC.Constraint] m
  ) =>
  EAC.PType primTy ->
  m (EAC.PType primTy)
reparameterize pty = do
  param <- freshParam
  -- Typing constraint: m >= 0
  addConstraint (EAC.Constraint [EAC.ConstraintVar 1 param] (EAC.Gte 0))
  case pty of
    EAC.PPrimT p -> pure (EAC.PPrimT p)
    EAC.PSymT _ sym -> pure (EAC.PSymT param sym)
    EAC.PArrT _ a b -> pure (EAC.PArrT param a b)

unificationConstraints ::
  forall m primTy.
  (HasWriter "constraints" [EAC.Constraint] m) =>
  EAC.PType primTy ->
  EAC.PType primTy ->
  m ()
unificationConstraints (EAC.PPrimT _) (EAC.PPrimT _) =
  -- TODO: Check p1 == p2 here?
  pure ()
unificationConstraints (EAC.PSymT a _) (EAC.PSymT b _) =
  addConstraint (constraint a b)
unificationConstraints (EAC.PArrT a aArg aRes) (EAC.PArrT b bArg bRes) = do
  addConstraint (constraint a b)
  unificationConstraints aArg bArg
  unificationConstraints aRes bRes
unificationConstraints _ _ =
  -- TODO: Throw an error here?
  pure mempty

constraint :: EAC.Param -> EAC.Param -> EAC.Constraint
constraint a b =
  (EAC.Constraint [EAC.ConstraintVar 1 a, EAC.ConstraintVar (- 1) b] (EAC.Eq 0))

--unificationConstraints x y = error ("cannot unify " <> show x <> " with " <> show y)

-- Generate boxing & typing constraints.
-- In one pass to avoid keeping extra maps.
-- TODO ∷ Change occurrenceMap to a reader at this point in the code
boxAndTypeConstraint ::
  forall m primTy primVal.
  ( HasState "path" EAC.Path m,
    HasState "varPaths" EAC.VarPaths m,
    HasState "nextParam" EAC.Param m,
    HasWriter "constraints" [EAC.Constraint] m,
    HasReader "occurrenceMap" EAC.OccurrenceMap m
  ) =>
  Core.Parameterisation primTy primVal ->
  EAC.ParamTypeAssignment primTy ->
  Erased.Term primVal ->
  m (EAC.RPT primVal, EAC.PType primTy)
boxAndTypeConstraint parameterisation parameterizedAssignment term = do
  let rec = boxAndTypeConstraint parameterisation parameterizedAssignment
      arrow (x :| []) = EAC.PPrimT x
      arrow (x :| (y : ys)) = EAC.PArrT 0 (EAC.PPrimT x) (arrow (y :| ys))
  varPaths <- get @"varPaths"
  param <- addPath
  path <- get @"path"
  -- Boxing constraint.
  addConstraint (EAC.Constraint (EAC.ConstraintVar 1 <$> path) (EAC.Gte 0))
  case term of
    Erased.Prim p ->
      pure
        ( EAC.RBang 0 (EAC.RPrim p),
          undefined {-arrow (Core.typeOf parameterisation p)-}
        )
    Erased.Var sym -> do
      -- Boxing constraint.
      case varPaths Map.!? sym of
        Just loc ->
          addConstraint
            ( EAC.Constraint
                (EAC.ConstraintVar 1 <$> dropWhile (< loc) path)
                (EAC.Eq 0)
            )
        Nothing ->
          addConstraint
            (EAC.Constraint (EAC.ConstraintVar 1 <$> path) (EAC.Eq 0))
      let origParamTy = parameterizedAssignment Map.! sym
      -- Typing constraint: variable occurrences.
      occurrenceMap <- ask @"occurrenceMap"
      let occurrences = occurrenceMap Map.! sym
          origBangParam = bangParam origParamTy
      -- If non-linear (>= 2 occurrences), original type must be of form !A.
      when (occurrences >= 2) $
        addConstraint
          (EAC.Constraint [EAC.ConstraintVar 1 origBangParam] (EAC.Gte 1))
      -- Calculate parameterized type for subterm.
      paramTy <- reparameterize origParamTy
      -- Typing constraint: m = k + n ⟹ k + n - m = 0
      -- where n = param
      -- and   k = origBangParam
      -- and   m = bangParam paramTy
      addConstraint
        ( EAC.Constraint
            [ EAC.ConstraintVar (- 1) (bangParam paramTy),
              EAC.ConstraintVar 1 origBangParam,
              EAC.ConstraintVar 1 param
            ]
            (EAC.Eq 0)
        )
      -- Typing constraint: m >= 0
      addConstraint
        (EAC.Constraint [EAC.ConstraintVar 1 (bangParam paramTy)] (EAC.Gte 0))
      -- Return parameterized term.
      pure (EAC.RBang param (EAC.RVar sym), paramTy)
    Erased.Lam sym body -> do
      nextParam <- getNextParam
      modify' @"varPaths" (Map.insert sym nextParam)
      (body, bodyTy) <- rec body
      -- Calculate parameterized type for subterm.
      lamTyParam <- freshParam
      -- Typing constraint: m >= 0
      addConstraint (EAC.Constraint [EAC.ConstraintVar 1 lamTyParam] (EAC.Gte 0))
      let argTy = parameterizedAssignment Map.! sym
          lamTy = EAC.PArrT lamTyParam argTy bodyTy
      -- Calculate final type.
      resTy <- reparameterize lamTy
      -- Typing constraint: m = 0
      addConstraint (EAC.Constraint [EAC.ConstraintVar 1 lamTyParam] (EAC.Eq 0))
      -- Typing constraint: m = k + n ⟹ k + n - m = 0
      -- where k = bangParam lamTy ∈ ℤ
      -- and   m = bangParam resTy ∈ ℤ
      -- and   n = param           ∈ ℤ
      addConstraint
        ( EAC.Constraint
            [ EAC.ConstraintVar (- 1) (bangParam resTy),
              EAC.ConstraintVar 1 (bangParam lamTy),
              EAC.ConstraintVar 1 param
            ]
            (EAC.Eq 0)
        )
      -- Typing constraint: m >= 0
      addConstraint
        (EAC.Constraint [EAC.ConstraintVar 1 (bangParam resTy)] (EAC.Gte 0))
      -- Return parameterized term.
      pure (EAC.RBang param (EAC.RLam sym body), resTy)
    Erased.Pair _s _t -> do
      undefined
    Erased.Unit -> do
      undefined
    Erased.Let _s _t _b -> do
      undefined
    Erased.App a b -> do
      (a, aTy) <- rec a
      let EAC.PArrT bangA argTy resTy = aTy
      put @"path" path
      put @"varPaths" varPaths
      (b, bTy) <- rec b
      -- Calculate parameterized type for subterm.
      appTy <- reparameterize resTy
      -- Typing constraint: U(A₁, A₂) ∪ m = 0
      -- where the terms are from:
      -- aTy = !^m(A₁ ⊸ B₁)
      -- bTy = A₂
      unificationConstraints argTy bTy
      addConstraint (EAC.Constraint [EAC.ConstraintVar 1 bangA] (EAC.Eq 0))
      -- Typing constraint: m = k + n ⟹ k + n - m = 0
      -- where n = param
      -- and   k = bangParam resTy
      -- and   m = bangParam appTy
      addConstraint
        ( EAC.Constraint
            [ EAC.ConstraintVar (- 1) (bangParam appTy),
              EAC.ConstraintVar 1 (bangParam resTy),
              EAC.ConstraintVar 1 param
            ]
            (EAC.Eq 0)
        )
      -- Typing constraint: m >= 0
      addConstraint
        (EAC.Constraint [EAC.ConstraintVar 1 (bangParam appTy)] (EAC.Gte 0))
      -- Return parameterized term.
      pure (EAC.RBang param (EAC.RApp a b), appTy)

-- Generate constraints.
generateTypeAndConstraints ::
  forall m primTy primVal.
  ( HasState "path" EAC.Path m,
    HasState "varPaths" EAC.VarPaths m,
    HasState "nextParam" EAC.Param m,
    HasState "typeAssignment" (Erased.TypeAssignment primTy) m,
    HasWriter "constraints" [EAC.Constraint] m,
    HasState "occurrenceMap" EAC.OccurrenceMap m,
    HasReader "occurrenceMap" EAC.OccurrenceMap m
  ) =>
  Core.Parameterisation primTy primVal ->
  Erased.Term primVal ->
  m (EAC.RPT primVal, EAC.ParamTypeAssignment primTy)
generateTypeAndConstraints parameterisation term = do
  parameterizedAssignment <- parameterizeTypeAssignment
  setOccurrenceMap term
  boxAndTypeConstraint parameterisation parameterizedAssignment term
    >>| second (const parameterizedAssignment)

generateConstraints ::
  ( HasState "path" EAC.Path m,
    HasState "varPaths" EAC.VarPaths m,
    HasState "nextParam" EAC.Param m,
    HasState "typeAssignment" (Erased.TypeAssignment primTy) m,
    HasWriter "constraints" [EAC.Constraint] m,
    HasState "occurrenceMap" EAC.OccurrenceMap m,
    HasReader "occurrenceMap" EAC.OccurrenceMap m
  ) =>
  Core.Parameterisation primTy primVal ->
  Erased.Term primVal ->
  m (EAC.RPT primVal)
generateConstraints parameterisation term =
  generateTypeAndConstraints parameterisation term
    >>| fst

{- Bracket Checker. -}
bracketChecker :: EAC.RPTO primVal -> Either EAC.BracketErrors ()
bracketChecker t = EAC.runEither (rec' t 0 mempty)
  where
    rec' (EAC.RBang changeBy (EAC.RVar sym)) n map =
      let f x
            | changeBy + n + x == 0 = pure ()
            | changeBy + n + x > 0 = throw @"typ" EAC.TooManyOpenV
            | otherwise = throw @"typ" EAC.TooManyClosingV
       in case map Map.!? sym of
            Just x -> f x
            Nothing -> f 0
    rec' (EAC.RBang changeBy t) n map
      | changeBy + n < 0 = throw @"typ" EAC.TooManyClosing
      | otherwise =
        let n' = changeBy + n
         in case t of
              EAC.RLam s t -> rec' t n' (Map.insert s (negate n') map)
              EAC.RApp t1 t2 -> rec' t1 n' map >> rec' t2 n' map
              EAC.RVar _ -> error "already is matched"
              EAC.RPrim _ -> pure ()

bracketCheckerErr :: EAC.RPTO primVal -> Either (EAC.Errors primTy primVal) ()
bracketCheckerErr t = left EAC.Brack (bracketChecker t)

{- Type Checker. -}
-- Precondition ∷ all terms inside of RPTO must be unique
typChecker ::
  forall primTy primVal.
  (Eq primTy) =>
  Core.Parameterisation primTy primVal ->
  EAC.RPTO primVal ->
  EAC.ParamTypeAssignment primTy ->
  Either (EAC.TypeErrors primTy primVal) ()
typChecker parameterisation t typAssign = EAC.runEither (() <$ rec' t typAssign)
  where
    arrow (x :| []) = EAC.PPrimT x
    arrow (x :| (y : ys)) = EAC.PArrT 0 (EAC.PPrimT x) (arrow (y :| ys))
    rec' (EAC.RBang bangVar (EAC.RVar s)) assign =
      case assign Map.!? s of
        Nothing -> throw @"typ" EAC.MissingOverUse
        Just t -> do
          newTyp <- addParamPos bangVar t
          if
              | bangParam t > 0 -> pure (assign, newTyp)
              | otherwise -> pure (Map.delete s assign, newTyp)
    rec' (EAC.RBang bangApp term@(EAC.RApp t1 t2)) assign = do
      (newAssign, type1) <- rec' t1 assign
      (newAssign', type2) <- rec' t2 newAssign
      case type1 of
        EAC.PArrT _ arg result
          | arg == type2 -> do
            newTyp <- addParamPos bangApp result
            pure (newAssign', newTyp)
          | otherwise -> throw @"typ" (EAC.MisMatchArguments arg type2 term)
        t@_ -> throw @"typ" (EAC.TypeIsNotFunction t)
    rec' (EAC.RBang x (EAC.RLam s t)) assign = do
      (newAssign, bodyType) <- rec' t assign
      case assign Map.!? s of
        Just arg -> pure (newAssign, EAC.PArrT x arg bodyType)
        Nothing -> throw @"typ" EAC.MissingOverUse
    rec' (EAC.RBang _bangVar (EAC.RPrim _p)) assign =
      pure
        ( assign,
          undefined {-arrow (Core.typeOf parameterisation _p)-}
        )

typCheckerErr ::
  forall primTy primVal.
  (Eq primTy) =>
  Core.Parameterisation primTy primVal ->
  EAC.RPTO primVal ->
  EAC.ParamTypeAssignment primTy ->
  Either (EAC.Errors primTy primVal) ()
typCheckerErr parameterisation t typeAssign =
  left EAC.Typ (typChecker parameterisation t typeAssign)

{- Utility. -}

-- The outer bang parameter.
bangParam :: forall primTy. EAC.PType primTy -> EAC.Param
bangParam (EAC.PPrimT _) = 0
bangParam (EAC.PSymT param _) = param
bangParam (EAC.PArrT param _ _) = param

putParam :: forall primTy. EAC.Param -> EAC.PType primTy -> EAC.PType primTy
putParam _ (EAC.PPrimT p) = EAC.PPrimT p
putParam p (EAC.PSymT _ s) = EAC.PSymT p s
putParam p (EAC.PArrT _ t1 t2) = EAC.PArrT p t1 t2

-- putParamPos ∷ Param → PType → PType
addParamPos ::
  forall m primTy primVal.
  HasThrow "typ" (EAC.TypeErrors primTy primVal) m =>
  EAC.Param ->
  EAC.PType primTy ->
  m (EAC.PType primTy)
addParamPos _ (EAC.PPrimT p) = pure (EAC.PPrimT p)
addParamPos toAdd (EAC.PSymT p s)
  | toAdd + p < 0 = throw @"typ" EAC.TooManyHats
  | otherwise = pure (EAC.PSymT (toAdd + p) s)
addParamPos toAdd (EAC.PArrT p t1 t2)
  | toAdd + p < 0 = throw @"typ" EAC.TooManyHats
  | otherwise = pure (EAC.PArrT (toAdd + p) t1 t2)

-- | Get the next fresh parameter
getNextParam :: forall b f. (HasState "nextParam" b f, Enum b) => f b
getNextParam = get @"nextParam"

-- | Generate fresh parameter.
freshParam :: forall m. (HasState "nextParam" EAC.Param m) => m EAC.Param
freshParam = do
  param <- get @"nextParam"
  put @"nextParam" (succ param)
  pure param

-- Append to path.
addPath ::
  (HasState "nextParam" EAC.Param m, HasState "path" EAC.Path m) => m EAC.Param
addPath = do
  param <- freshParam
  modify' @"path" (<> [param])
  pure param

-- Add constraint.
addConstraint :: HasWriter "constraints" [EAC.Constraint] m => EAC.Constraint -> m ()
addConstraint con = tell @"constraints" [con]

-- Execute with prior assignment.
execWithAssignment ::
  Erased.TypeAssignment primTy -> EAC.EnvConstraint primTy a -> (a, EAC.Env primTy)
execWithAssignment assignment (EAC.EnvCon env) =
  runState env (EAC.Env [] mempty assignment 0 [] mempty)
