module Juvix.FrontendDesugar.RemoveDo.Extend
  ( module Juvix.FrontendDesugar.RemoveDo.Extend,
    module Juvix.FrontendDesugar.RemovePunned.Extend,
  )
where

import Juvix.Frontend.Types.Base
import Juvix.FrontendDesugar.RemovePunned.Extend hiding (extendDo, extendExpression)
import qualified Juvix.FrontendDesugar.RemovePunned.Extend as Ext
import Juvix.Library

extendExpression :: ExtExpression
extendExpression = Ext.extendExpression {typeDo = Nothing}
