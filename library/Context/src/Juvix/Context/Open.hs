module Juvix.Context.Open where

import Juvix.Library

data T = Explicit | Implicit deriving (Show, Read, Eq, Generic)

data TName a = TName
  { open :: T,
    name :: a
  }
  deriving (Show, Read, Eq, Generic)
