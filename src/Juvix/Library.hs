module Juvix.Library ( module Protolude
                     , module Capability.State
                     , module Capability.Reader
                     , module Capability.Error
                     , module Capability.Writer
                     , module Capability.Stream
                     , (∨), (∧), (|<<), (>>|), (|>)
                     , traverseM, Symbol, intern, unintern
                     ) where

import           Capability.Error
import           Capability.Reader
import           Capability.State
import           Capability.Stream
import           Capability.Writer
import           Data.String       (fromString)
import qualified Data.Text         as T
import           Prelude           (Show (..), String)
import           Protolude         hiding ((:.:), Constraint, Fixity (..),
                                    MonadError (..), MonadReader (..),
                                    MonadState (..), SomeSymbol, Symbol, ask,
                                    asks, catch, catchJust, get, gets, local,
                                    modify, pass, put, reader, state)

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

(|>) ∷ a → (a → b) → b
(|>) = (&)
infixl 1 |>

traverseM ∷ (Monad m, Traversable m, Applicative f)
          ⇒ (a1 → f (m a2))
          → m a1
          → f (m a2)
traverseM f = fmap join . traverse f

instance Show ((→) a b) where
  show _ = "fun"


newtype Symbol = Sym Text deriving (Eq, Hashable)

instance Show Symbol where
  show (Sym t) = T.unpack t

instance IsString Symbol where
  fromString = intern

intern ∷ String → Symbol
intern = Sym . T.pack

unintern ∷ Symbol → String
unintern (Sym s) = T.unpack s
