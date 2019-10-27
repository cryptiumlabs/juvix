module Juvix.Core.EAC.Check where

import qualified Data.Map.Strict as Map
import Juvix.Core.EAC.ConstraintGen
import Juvix.Core.EAC.Solve
import Juvix.Core.EAC.Types
import Juvix.Library hiding (link, reduce)

validEal ∷ Term → TypeAssignment → IO (Either Errors (RPT, ParamTypeAssignment))
validEal term typMap = do
  let ((rpt, typ), env) =
        execWithAssignment typMap $
          generateTypeAndConstraints term
      constraint = constraints env
  -- Z3 constraint assignment
  assignment ← getConstraints constraint
  pure $
    case assignment of
      Just x →
        let valAssignment = assignTerm x rpt
            typAssignment = assignType x typ
         in -- TODO: If an assignment was generated, but either of these checks fails,
            -- we must have made a mistake in constraint generation.
            -- <|> doesn't work, find out why and refactor code later
            case bracketCheckerErr valAssignment of
              Left e → Left e
              Right _ →
                case typCheckerErr valAssignment typAssignment of
                  Left e → Left e
                  Right _ → Right (valAssignment, typAssignment)
      Nothing →
        Left (Brack InvalidAssignment)

assignType ∷ [Integer] → ParamTypeAssignment → ParamTypeAssignment
assignType assignment typ = typ >>| placeVals
  where
    conMap = Map.fromList (zip [0 ..] (fromInteger <$> assignment))
    placeVals (PArrT p t1 t2) = PArrT (conMap Map.! p) (placeVals t1) (placeVals t2)
    placeVals (PSymT p s) = PSymT (conMap Map.! p) s

assignTerm ∷ [Integer] → RPT → RPT
assignTerm assignment syn = placeVals syn
  where
    conMap = Map.fromList (zip [0 ..] (fromInteger <$> assignment))
    placeVals (RBang i t) =
      RBang
        (conMap Map.! i)
        ( case t of
            RLam s t → RLam s (placeVals t)
            RApp t1 t2 → RApp (placeVals t1) (placeVals t2)
            RVar s → RVar s
        )
