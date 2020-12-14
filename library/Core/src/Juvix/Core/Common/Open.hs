module Juvix.Core.Common.Open where

import Juvix.Library

data T = Explicit | Implicit deriving (Show, Eq, Data, Generic)
