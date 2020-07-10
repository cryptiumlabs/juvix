-- |
-- - This pass removes the =Do= form
--  + Belongs to Table
--    | Changed | Is a Sum Type of |
--    |---------+------------------|
--    | Do      | Expression       |
-- - Thus one does not have to ever deal with
--   #+begin_src haskell
--     data Do
--       = Do'' (NonEmpty DoBody)
--       deriving (Show, Generic, NFData)
--
--     data DoBody
--       = DoBody
--         { doBodyName :: Maybe NameSymb,
--           doBodyExpr :: Expression
--         }
--       deriving (Show, Generic, NFData)
--   #+end_src
--   after this pass
module Juvix.FrontendDesugar.RemoveDo.Extend
  ( module Juvix.FrontendDesugar.RemoveDo.Extend,
    module Juvix.FrontendDesugar.RemovePunned.Extend,
  )
where

import Juvix.Frontend.Types.Base
import Juvix.FrontendDesugar.RemovePunned.Extend hiding (extendExpression)
import qualified Juvix.FrontendDesugar.RemovePunned.Extend as Ext
import Juvix.Library

extendExpression :: ExtExpression
extendExpression = Ext.extendExpression {typeDo = Nothing}
