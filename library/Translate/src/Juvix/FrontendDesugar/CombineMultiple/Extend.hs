-- |
-- - This pass changes =FunctionLike=, =Function=, and =Let=
--   + Belongs to Table
--     | Changed      | Is a Sum Type of |
--     |--------------+------------------|
--     | Function     | TopLevel         |
--     | Let          | Expression       |
--     | FunctionLike | Function ∧ Let   |
-- - _Function Like changes_
--   + Function Like now looks like
--     #+begin_src haskell
--       data FunctionLike a
--         = Like
--           { functionLikeArgs :: [Arg]
--           , functionLikeBody :: a
--           }
--         deriving (Show, Generic, NFData)
--     #+end_src
--     * This pass removes the =Name= from the previous transform
-- - _Let changes_
--   + Let now looks like
--     #+begin_src haskell
--       data Let
--         = LetGroup
--           { letName :: Symbol
--           , letBindings :: NonEmpty (FunctionLike Expression)
--           , letBody :: Expression
--           }
--         deriving (Show, Generic, NFData)
--     #+end_src
--     * In this pass we add =Name= from the previous Let type
-- - _Function changes_
--   + Function now looks like
--     #+begin_src haskell
--       data Function
--         = Func Symbol (NonEmpty (FunctionLike Expression))
--     #+end_src
--     * Namely this version adds symbol to the previous pass
module Juvix.FrontendDesugar.CombineMultiple.Extend
  ( module Juvix.FrontendDesugar.CombineMultiple.Extend,
    module Juvix.FrontendDesugar.RemoveCond.Extend,
  )
where

import qualified Extensible as Extension
import Juvix.Frontend.Types.Base
import Juvix.FrontendDesugar.RemoveCond.Extend hiding
  ( extendFunction,
    extendFunctionLike,
    extendLet,
  )
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
