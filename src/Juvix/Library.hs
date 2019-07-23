{-# LANGUAGE TemplateHaskell #-}
module Juvix.Library ( module Protolude
                     , module Capability.State
                     , module Capability.Reader
                     , module Capability.Error
                     , (∨), (∧)
                     ) where

import Protolude hiding (state, get, put, modify, gets, MonadState(..), (:.:)
                        , reader, local, ask, asks, MonadReader(..), Fixity(..)
                        , catchJust, catch, MonadError(..)
                        , Constraint)
import Capability.State
import Capability.Reader
import Capability.Error

(∨) :: Bool → Bool → Bool
(∨) = (||)
infixr 2 ∨

(∧) :: Bool → Bool → Bool
(∧) = (&&)
infixr 3 ∧
