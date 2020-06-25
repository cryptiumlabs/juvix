module Juvix.FrontendContextualise.EraseTypeAliases.Extend
  ( module Juvix.FrontendContextualise.EraseTypeAliases.Extend,
    module Juvix.FrontendDesugar.RemoveDo.Extend,
  )
where

--FIXME:change to the module of the last pass

import Juvix.Frontend.Types.Base
import Juvix.FrontendDesugar.RemoveDo.Extend hiding (extendExpression) --FIXME
import qualified Juvix.FrontendDesugar.RemoveDo.Extend as Ext --FIXME
import Juvix.Library

-- TODO fill this out for real
extendExpression :: ExtExpression
extendExpression = Ext.extendExpression {typeDo = Nothing}
