-- |
-- - The HashMap for the codebase.
-- - Basically just imports Data.HashMap.Strict
--   + While giving the operation =!?=.
-- - Every hash in the code base should use this, except when it needs
--   to compare keys by the =Ordering= metric instead.
module Juvix.Library.HashMap
  ( module Data.HashMap.Strict,
    Map,
    (!?),
  )
where

import Data.HashMap.Strict
import Data.Hashable (Hashable)
import Protolude (Eq, Maybe)

type Map = HashMap

(!?) ∷ (Eq k, Hashable k) ⇒ HashMap k v → k → Maybe v
(!?) m k = lookup k m
