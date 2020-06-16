module Juvix.FrontendDesugar.RemovePunned.Extend
  ( module Juvix.FrontendDesugar.RemoveSignature.Extend,
    module Juvix.FrontendDesugar.RemovePunned.Extend,
  )
where

import Juvix.Frontend.Types.Base
import Juvix.FrontendDesugar.RemoveSignature.Extend hiding (extendNameSet)
import qualified Juvix.FrontendDesugar.RemoveSignature.Extend as Ext
import Juvix.Library

extendNameSet :: ExtNameSet
extendNameSet = Ext.extendNameSet {typePunned = Nothing}
