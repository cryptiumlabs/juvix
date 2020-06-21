module Juvix.FrontendDesugar.RemoveSignature.Extend
  ( module Juvix.FrontendDesugar.RemoveSignature.Extend,
    module Juvix.FrontendDesugar.CombineMultiple.Extend,
  )
where

import qualified Extensible as Extension
import Juvix.Frontend.Types.Base
import Juvix.FrontendDesugar.CombineMultiple.Extend hiding (extendFunction, extendTopLevel)
import qualified Juvix.FrontendDesugar.CombineMultiple.Extend as Ext
import Juvix.Library

extendTopLevel :: ExtTopLevel
extendTopLevel = Ext.extendTopLevel {typeSignature = Nothing}

extendFunction :: Extension.TypeQ -> ExtFunction
extendFunction arg =
  -- have to use the default here as we have a function in the previous code
  defaultExtFunction
    { typeFunc = Nothing,
      typeFunctionX =
        [ ( "Func",
            [ [t|Symbol|],
              [t|NonEmpty (FunctionLike' $arg (Expression' $arg))|],
              [t|Maybe (Signature' $arg)|]
            ]
          )
        ]
    }

-- TODO ∷ once we get a signature for let add that here too
