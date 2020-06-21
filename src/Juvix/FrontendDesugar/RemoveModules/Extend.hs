module Juvix.FrontendDesugar.RemoveModules.Extend
  ( module Juvix.Frontend.Types.Extend,
    module Juvix.FrontendDesugar.RemoveModules.Extend,
  )
where

import Juvix.Frontend.Types.Base
import qualified Juvix.Frontend.Types.Extend as Ext
import Juvix.Frontend.Types.Extend hiding (extendExpression, extendTopLevel)
import Juvix.Library hiding (Product, Sum)

extendExpression :: ExtExpression
extendExpression = Ext.extendExpression {typeModuleE = Nothing}

extendTopLevel :: ExtTopLevel
extendTopLevel = Ext.extendTopLevel {typeModule = Nothing}
