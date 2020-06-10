module Juvix.FrontendDesugar.Abstractions where

import Juvix.Frontend.Types.Base
import Juvix.Library hiding (Product, Sum)
import qualified Extensible as Extension

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
