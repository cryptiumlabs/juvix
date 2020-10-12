-- |
-- - This pass changes =FunctionLike=
--   + Belongs to Table
--     | Changed      | Is a Sum Type of |
--     |--------------+------------------|
--     | FunctionLike | Function ∧ Let   |
-- - _Function Like changes_
--   + Function Like now looks like
--     #+begin_src haskell
--       data FunctionLike a
--         = Like
--           { functionLikeName :: Symbol
--           , functionLikeArgs :: [Arg]
--           , functionLikeBody :: a
--           }
--         deriving (Show, Generic, NFData)
--     #+end_src
--     * This pass removes the =GuardBody= from the body.
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
