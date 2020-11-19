{-# OPTIONS_GHC -fdefer-typed-holes #-}

-- | Calculate mutually-recursive groups of definitions.
module Juvix.Core.Common.Context.RecGroups
where

import Juvix.Core.Common.Context.Types
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol

-- | Sorts a context by dependency order. Each element of the output is
-- a mutually-recursive group, whose elements depend only on each other and
-- elements of previous groups. The first element of each pair is its
-- fully-qualified name.
recGroups :: T a b c -> [NonEmpty (NameSymbol.T, Definition a b c)]
recGroups = _ -- TODO

