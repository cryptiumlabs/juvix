-- |
-- - This pass removes the =Infix= form
--   + Belongs to Table
--     | Changed | Is a Sum Type of |
--     |---------+------------------|
--     | Infix   | Expression       |
-- - Thus one does not have to ever deal with
--   #+begin_src haskell
--     data Infix
--       = Inf
--         { infixLeft  :: Expression
--         , infixOp    :: NameSymb
--         , infixRight :: Expression
--         }
--       deriving (Show, Generic, NFData)
--   #+end_src
module Juvix.FrontendContextualise.InfixPrecedence.Extend
  ( module Juvix.FrontendContextualise.ModuleOpen.Extend,
    module Juvix.FrontendContextualise.InfixPrecedence.Extend,
  )
where

import Juvix.Frontend.Types.Base
import Juvix.FrontendContextualise.ModuleOpen.Extend hiding (extendExpression)
import qualified Juvix.FrontendContextualise.ModuleOpen.Extend as Ext
import Juvix.Library

extendExpression :: ExtExpression
extendExpression = Ext.extendExpression {typeInfix = Nothing}
