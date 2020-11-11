-- |
-- Types to support partial application and polymorphic primitives.
module Juvix.Core.Application where

import Juvix.Library
import qualified Juvix.Library.Usage as Usage

-- |
-- A primitive along with its type, and possibly some arguments.
data Return ty term
  = -- | Partially applied primitive holding the arguments already given
    Cont
      { -- | head of application
        fun :: Take ty term,
        -- | arguments
        args :: [Take ty term],
        -- | number of arguments still expected
        numLeft :: Natural
      }
  | -- | A primitive with no arguments
    Return
      { retType :: ty,
        retTerm :: term
      }
  deriving (Show, Eq, Generic)

-- |
-- An argument to a partially applied primitive, which must be
-- fully-applied itself.
data Take ty term
  = Take
      { usage :: Usage.T,
        type' :: ty,
        term :: term
      }
  deriving (Show, Eq, Generic)
