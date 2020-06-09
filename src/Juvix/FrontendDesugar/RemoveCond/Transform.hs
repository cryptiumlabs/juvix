module Juvix.FrontendDesugar.RemoveCond.Transform where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Juvix.FrontendDesugar.RemoveCond.Types as New
import qualified Juvix.FrontendDesugar.RemoveGuard.Types as Old
import Juvix.Library

transformtTopLevel :: Old.TopLevel -> New.TopLevel
transformtTopLevel (Old.Type t) = New.Type undefined

transFormExpression :: Old.Expression -> New.Expression
transFormExpression = undefined

transFormCond :: Old.Cond Old.Expression -> New.Expression
transFormCond (Old.C xs) =
  foldr f (fList last []) xs
  where
    fList (Old.CondExpression pred body) falses =
      boolean "True" (transFormExpression body)
        |> (NonEmpty.:| falses)
        |> New.Match'' (transFormExpression pred)
        |> New.Match
    f conds falseBranch =
      fList conds [boolean "false" falseBranch]
    last =
      NonEmpty.last xs
    boolean symb body =
      New.MatchCon (symb :| []) []
        |> flip New.MatchLogic Nothing
        |> flip New.MatchL body
