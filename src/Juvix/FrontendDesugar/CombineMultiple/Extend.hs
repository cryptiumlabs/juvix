module Juvix.FrontendDesugar.CombineMultiple.Extend
  ( module Juvix.FrontendDesugar.CombineMultiple.Extend,
    module Juvix.FrontendDesugar.RemoveCond.Extend,
  )
where

import qualified Extensible as Extension
import Juvix.Frontend.Types.Base
import Juvix.FrontendDesugar.RemoveCond.Extend hiding (extendFunction, extendFunctionLike, extendLet)
import qualified Juvix.FrontendDesugar.RemoveCond.Extend as Ext
import Juvix.Library

extendLet :: Extension.TypeQ -> ExtLet
extendLet arg =
  Ext.extendLet
    { typeLet'' = Nothing,
      typeLetX =
        [ ( "LetGroup",
            [ ("letName", [t|Symbol|]),
              ("letBindings", [t|NonEmpty (FunctionLike' $arg (Expression' $arg))|]),
              ("letBody", [t|Expression' $arg|])
            ]
          )
        ]
    }

extendFunction :: Extension.TypeQ -> ExtFunction
extendFunction arg =
  Ext.extendFunction
    { typeFunc = Nothing,
      typeFunctionX =
        [ ( "Func",
            [ [t|Symbol|],
              [t|NonEmpty (FunctionLike' $arg (Expression' $arg))|]
            ]
          )
        ]
    }

-- sadly have to modify on the default :(
extendFunctionLike :: Extension.TypeQ -> Extension.TypeQ -> ExtFunctionLike
extendFunctionLike arg a =
  defaultExtFunctionLike
    { typeLike = Nothing,
      typeFunctionLikeX =
        [ ( "Like",
            [ ("functionLikeArgs", [t|[Arg' $arg]|]),
              ("functionLikeBody", a)
            ]
          )
        ]
    }
