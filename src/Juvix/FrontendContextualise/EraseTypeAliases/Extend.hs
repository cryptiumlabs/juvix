module Juvix.FrontendContextualise.EraseTypeAliases.Extend
  ( module Juvix.FrontendContextualise.EraseTypeAliases.Extend,
    module Juvix.FrontendDesugar.RemoveDo.Extend,
  )
where

import Juvix.Frontend.Types.Base
import Juvix.Frontend.FrontendContextualise.EraseTypeAliases.Extend hiding (extendDo, extendExpression)
import qualified Juvix.FrontendDesugar.RemovePDo.Extend as Ext
import Juvix.Library

extendExpression :: ExtExpression
extendExpression = Ext.extendExpression {typeDo = Nothing}
