module Juvix.FrontendDesugar.Abstractions where

import qualified Extensible as Extension
import Juvix.Frontend.Types.Base
import Juvix.Library hiding (Product, Sum)

functionLikeNoCond :: Extension.TypeQ -> Extension.TypeQ -> ExtFunctionLike
functionLikeNoCond arg a =
  defaultExtFunctionLike
    { typeLike = Nothing,
      typeFunctionLikeX =
        [ ( "Like",
            [ ("functionLikeName", [t|Symbol|]),
              ("functionLikeArgs", [t|[Arg' $arg]|]),
              ("functionLikeBody", a)
            ]
          )
        ]
    }
