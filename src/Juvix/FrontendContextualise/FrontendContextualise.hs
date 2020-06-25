module Juvix.FrontendContextualise where

import qualified Juvix.FrontendContextualise.Environment as Env
import qualified Juvix.FrontendContextualise.InfixPrecedence.Transform as Infix
import qualified Juvix.FrontendContextualise.Types as Target
import qualified Juvix.FrontendDesugar.RemoveDo.Types as Initial
import Juvix.Library

contextualise :: [Initial.TopLevel] -> Env.Context Target.TopLevel
contextualise initials =
  Env.forKey
    (linearContextualisation initials)
    [ -- all passes in order
      Infix.transformTopLevel
    ]

linearContextualisation :: [Initial.TopLevel] -> Env.Context Target.TopLevel
linearContextualisation = undefined
