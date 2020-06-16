module Juvix.FrontendDesugar.RemoveCond.Extend
  ( module Juvix.FrontendDesugar.RemoveGuard.Extend,
    module Juvix.FrontendDesugar.RemoveCond.Extend,
  )
where

import Juvix.Frontend.Types.Base
import Juvix.FrontendDesugar.RemoveGuard.Extend hiding (extendExpression)
import qualified Juvix.FrontendDesugar.RemoveGuard.Extend as Ext
import Juvix.Library

extendExpression :: ExtExpression
extendExpression = Ext.extendExpression {typeCond = Nothing}
