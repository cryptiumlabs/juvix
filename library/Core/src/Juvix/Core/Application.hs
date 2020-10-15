-- |
-- - This module represents the type which will be sent to the
--   parameterisation
-- - the =Take= type is what a parameterisation will take coming in
-- - the =Return= type is what will be handed back to Core to evaluate
--   and decide on the next steps. If this is a =Left= type checking
--   has failed, if it's a =Right= then type checking will continue
module Juvix.Core.Application where

import qualified Juvix.Library.Usage as Usage
import Juvix.Library

data Return ty term
  = -- arguments left
    Cont
      { fun :: Take ty term,
        args :: [Take ty term],
        numLeft :: Natural
      }
  | Return term
  deriving (Show, Eq, Generic)

data Take ty term
  = Take
      { usage :: Usage.T,
        type' :: ty,
        term :: term
      }
  deriving (Show, Eq, Generic)
