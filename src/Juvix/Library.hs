module Juvix.Library ( module Protolude
                     , module Capability.State
                     , module Capability.Reader
                     , module Capability.Error
                     , (∨), (∧), (|<<), (>>|)
                     ) where

import           Capability.Error
import           Capability.Reader
import           Capability.State
import           Protolude         hiding ((:.:), Constraint, Fixity (..),
                                    MonadError (..), MonadReader (..),
                                    MonadState (..), ask, asks, catch,
                                    catchJust, get, gets, local, modify, put,
                                    reader, state)

(∨) ∷ Bool → Bool → Bool
(∨) = (||)
infixr 2 ∨

(∧) ∷ Bool → Bool → Bool
(∧) = (&&)
infixr 3 ∧

(|<<) ∷ ∀ a b f . (Functor f) ⇒ (a → b) → f a → f b
(|<<) = fmap
infixl 4 |<<

(>>|) ∷ ∀ a b f . (Functor f) ⇒ f a → (a → b) → f b
(>>|) = flip fmap
infixl 4 >>|
