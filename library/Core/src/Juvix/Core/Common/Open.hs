module Juvix.Core.Common.Open where

import Juvix.Library

data T = Explicit | Implicit deriving (Show, Eq, Generic)

data TName a = TName
  { open :: T,
    name :: a
  }
  deriving (Show, Eq, Generic)
