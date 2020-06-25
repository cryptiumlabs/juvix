module Juvix.FrontendContextualise.InfixPrecedence.Extend
  ( module Juvix.FrontendDesugar.RemoveDo.Extend,
    module Juvix.FrontendContextualise.InfixPrecedence.Extend,
  )
where

import Juvix.Frontend.Types.Base
import Juvix.FrontendDesugar.RemoveDo.Extend hiding (extendExpression, extendInfix)
import qualified Juvix.FrontendDesugar.RemoveDo.Extend as Ext
import Juvix.Library

extendExpression :: ExtExpression
extendExpression = Ext.extendExpression {typeInfix = Nothing}
