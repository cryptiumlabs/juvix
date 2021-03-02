-- |
-- - This pass removes the =Do= form
--  + Belongs to Table
--    | Changed | Is a Sum Type of |
--    |---------+------------------|
--    | Cond    | Expression       |
-- - Thus one does not have to ever deal with
--   #+begin_src haskell
--     data Cond a
--       = C (NonEmpty (CondLogic a))
--       deriving (Show, Generic, NFData)
--
--     data CondLogic a
--       = CondExpression
--           { condLogicPred :: Expression
--           , condLogicBody :: a
--           }
--       deriving (Show, Generic, NFData)
--   #+end_src
--   after this pass
module Juvix.FrontendDesugar.RemoveCond.Extend
  ( module Juvix.FrontendDesugar.RemoveHandlers.Extend,
    module Juvix.FrontendDesugar.RemoveCond.Extend,
  )
where

import Juvix.Frontend.Types.Base
import Juvix.FrontendDesugar.RemoveHandlers.Extend hiding (extendExpression)
import qualified Juvix.FrontendDesugar.RemoveHandlers.Extend as Ext
import Juvix.Library

extendExpression :: ExtExpression
extendExpression = Ext.extendExpression {typeCond = Nothing}
