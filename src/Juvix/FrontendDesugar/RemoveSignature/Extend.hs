module Juvix.FrontendDesugar.RemoveSignature.Extend
  ( module Juvix.FrontendDesugar.RemoveSignature.Extend,
    module Juvix.FrontendDesugar.RemoveCond.Extend,
  )
where

import qualified Extensible as Extension
import Juvix.Frontend.Types.Base
import Juvix.FrontendDesugar.RemoveCond.Extend hiding (extendFunction, extendTopLevel)
import qualified Juvix.FrontendDesugar.RemoveCond.Extend as Ext
import Juvix.Library

extendTopLevel :: ExtTopLevel
extendTopLevel = Ext.extendTopLevel {typeSignature = Nothing}

extendFunction :: Extension.TypeQ -> ExtFunction
extendFunction arg =
  Ext.extendFunction
    { typeFunc = Nothing,
      typeFunctionX =
        [ ( "Func",
            [ [t|FunctionLike' $arg (Expression' $arg)|],
              [t|Signature' $arg|]
            ]
          )
        ]
    }
