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
