-- |
-- - This pass removes the =Punned= form
--  + Belongs to Table
--    | Removed | Is a Sum Type of |
--    |---------+------------------|
--    | Punned  | NameSet          |
-- - Thus one does not have to ever deal with the =Punned= variant in
--   #+begin_src haskell
--     data NameSet t
--       = Punned NameSymb
--       | NonPunned NameSymb t
--       deriving (Show, Generic, NFData)
--   #+end_src
--   after this pass
module Juvix.FrontendDesugar.RemovePunned.Extend
  ( module Juvix.FrontendDesugar.RemoveSignature.Extend,
    module Juvix.FrontendDesugar.RemovePunned.Extend,
  )
where

import Juvix.Frontend.Types.Base
import Juvix.FrontendDesugar.RemoveSignature.Extend hiding (extendNameSet)
import qualified Juvix.FrontendDesugar.RemoveSignature.Extend as Ext
import Juvix.Library

extendNameSet :: ExtNameSet
extendNameSet = Ext.extendNameSet {typePunned = Nothing}
