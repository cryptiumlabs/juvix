-- |
-- - A constraint checker for EAC.
module Juvix.Core.EAC.Check where

import qualified Data.Map.Strict as Map
import qualified Juvix.Core.EAC.ConstraintGen as Constraint
import qualified Juvix.Core.EAC.Solve as Solve
import qualified Juvix.Core.EAC.Types as EAC
import qualified Juvix.Core.Erased.Types as Types
import Juvix.Core.Types
import Juvix.Library hiding (link, reduce)

validEal ∷
  ∀ primTy primVal.
  (Eq primTy) ⇒
  Parameterisation primTy primVal →
  Types.Term primVal →
  Types.TypeAssignment primTy →
  IO
    ( Either
        (EAC.Errors primTy primVal)
        (EAC.RPT primVal, EAC.ParamTypeAssignment primTy)
    )
validEal param term typMap = do
  let ((rpt, typ), env) =
        Constraint.execWithAssignment typMap $
          Constraint.generateTypeAndConstraints param term
      constraint = EAC.constraints env
  -- Z3 constraint assignment
  assignment ← Solve.getConstraints constraint
  pure $
    case assignment of
      Just x →
        let valAssignment = assignTerm x rpt
            typAssignment = assignType x typ
         in -- TODO: If an assignment was generated, but either of these checks fails,
            -- we must have made a mistake in constraint generation.
            -- <|> doesn't work, find out why and refactor code later
            case Constraint.bracketCheckerErr valAssignment of
              Left e → Left e
              Right _ →
                case Constraint.typCheckerErr param valAssignment typAssignment of
                  Left e → Left e
                  Right _ → Right (valAssignment, typAssignment)
      Nothing →
        Left (EAC.Brack EAC.InvalidAssignment)

assignType ∷ [Integer] → EAC.ParamTypeAssignment primTy → EAC.ParamTypeAssignment primTy
assignType assignment typ = typ >>| placeVals
  where
    conMap = Map.fromList (zip [0 ..] (fromInteger <$> assignment))
    placeVals (EAC.PPrimT p) =
      EAC.PPrimT p
    placeVals (EAC.PArrT p t1 t2) =
      EAC.PArrT (conMap Map.! p) (placeVals t1) (placeVals t2)
    placeVals (EAC.PSymT p s) =
      EAC.PSymT (conMap Map.! p) s

assignTerm ∷ [Integer] → EAC.RPT primVal → EAC.RPT primVal
assignTerm assignment syn = placeVals syn
  where
    conMap = Map.fromList (zip [0 ..] (fromInteger <$> assignment))
    placeVals (EAC.RBang i t) =
      EAC.RBang
        (conMap Map.! i)
        ( case t of
            EAC.RPrim p → EAC.RPrim p
            EAC.RLam s t → EAC.RLam s (placeVals t)
            EAC.RApp t1 t2 → EAC.RApp (placeVals t1) (placeVals t2)
            EAC.RVar s → EAC.RVar s
        )
