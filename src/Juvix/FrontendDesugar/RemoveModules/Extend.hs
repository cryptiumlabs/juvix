-- |
-- - This pass removes the =Module= and =ModuleE= form
--  + Belongs to Table
--    | Changed | Is a Sum Type of |
--    |---------+------------------|
--    | Module  | TopLevel         |
--    | ModuleE | Expression       |
-- - Thus one does not have to ever deal with
--   #+begin_src haskell
--     data Module
--       = Mod (FunctionLike (NonEmpty TopLevel))
--       deriving (Show, Generic, NFData)
--
--     data ModuleE
--       = ModE
--           { moduleEBindings :: FunctionLike (NonEmpty TopLevel)
--           , moduleEBody :: Expression
--           }
--       deriving (Show, Generic, NFData)
--   #+end_src
--   after this pass
module Juvix.FrontendDesugar.RemoveModules.Extend
  ( module Juvix.Frontend.Types.Extend,
    module Juvix.FrontendDesugar.RemoveModules.Extend,
  )
where

import Juvix.Frontend.Types.Base
import qualified Juvix.Frontend.Types.Extend as Ext
import Juvix.Frontend.Types.Extend hiding (extendExpression, extendTopLevel)
import Juvix.Library hiding (Product, Sum)

extendExpression :: ExtExpression
extendExpression = Ext.extendExpression {typeModuleE = Nothing}

extendTopLevel :: ExtTopLevel
extendTopLevel = Ext.extendTopLevel {typeModule = Nothing}
