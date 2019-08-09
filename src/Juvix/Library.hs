module Juvix.Library ( module Protolude
                     , module Capability.State
                     , module Capability.Reader
                     , module Capability.Error
                     , module Capability.Writer
                     , module Capability.Stream
                     , (∨), (∧), (|<<), (>>|), (|>)
                     ) where

import           Capability.Error
import           Capability.Reader
import           Capability.State
import           Capability.Writer
import           Capability.Stream
import           Protolude         hiding ((:.:), Constraint, Fixity (..),
                                    MonadError (..), MonadReader (..),
                                    MonadState (..), ask, asks, catch,
                                    catchJust, get, gets, local, modify, put,
                                    reader, state, pass)

(∨) ∷ Bool → Bool → Bool
(∨) = (||)
infixr 2 ∨

(∧) ∷ Bool → Bool → Bool
(∧) = (&&)
infixr 3 ∧

(|<<) ∷ ∀ a b f . (Functor f) ⇒ (a → b) → f a → f b
(|<<) = fmap
infixr 1 |<<

(>>|) ∷ ∀ a b f . (Functor f) ⇒ f a → (a → b) → f b
(>>|) = flip fmap
infixl 1 >>|

(|>) :: a → (a → b) → b
(|>) = (&)
infixl 1 |>
