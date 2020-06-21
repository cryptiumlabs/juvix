module Juvix.FrontendDesugar.RemoveGuard.Extend
  ( module Juvix.FrontendDesugar.RemoveModules.Extend,
    module Juvix.FrontendDesugar.RemoveGuard.Extend,
  )
where

import qualified Extensible as Extension
import Juvix.Frontend.Types.Base
import qualified Juvix.FrontendDesugar.Abstractions as Abstract
import Juvix.FrontendDesugar.RemoveModules.Extend hiding (extendFunctionLike)

extendFunctionLike :: Extension.TypeQ -> Extension.TypeQ -> ExtFunctionLike
extendFunctionLike = Abstract.functionLikeNoCond
