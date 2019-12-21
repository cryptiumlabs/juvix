-- |
-- - The standard Library for the project
--   + Thus all code will depend on this module without stating otherwise
-- - Is mostly =Protolude= except with a few changes
--   + _Additions_
--     * ∨   :: Serves as an or function
--     * ∧   :: Serves as an and function
--     * |<< :: Serves as a map function
--     * >>| :: Serves as the flip map function
--   + _Changes_
--     * The Capability library is imported and replaces the standard =MTL=
--       constructs in =Protolude=
module Juvix.Library
  ( module Protolude,
    module Capability.State,
    module Capability.Reader,
    module Capability.Error,
    module Capability.Writer,
    module Capability.Stream,
    module Numeric.Natural,
    module Juvix.Library.PrettyPrint,
    (∨),
    (∧),
    (|<<),
    (>>|),
    (|>),
    traverseM,
    Symbol,
    intern,
    unintern,
    unixTime,
    Flip (..),
    untilNothingNTimesM,
    untilNothing,
    sortOnFlip,
    uncurry3,
    curry3,
  )
where

import Capability.Error
import Capability.Reader
import Capability.State
import Capability.Stream
import Capability.Writer
import Data.Hashable (Hashable)
import Data.String (fromString)
import qualified Data.Text as T
import Data.Time.Clock.POSIX
import Juvix.Library.PrettyPrint
import Numeric.Natural
import Protolude hiding
  ( (:.:),
    Constraint,
    Fixity (..),
    MonadError (..),
    MonadReader (..),
    MonadState (..),
    SomeSymbol,
    Symbol,
    ask,
    asks,
    catch,
    catchJust,
    get,
    gets,
    local,
    modify,
    moduleName,
    pass,
    put,
    reader,
    state,
  )
import Prelude (Show (..), String)

(∨) ∷ Bool → Bool → Bool
(∨) = (||)

infixr 2 ∨

(∧) ∷ Bool → Bool → Bool
(∧) = (&&)

infixr 3 ∧

(|<<) ∷ ∀ a b f. (Functor f) ⇒ (a → b) → f a → f b
(|<<) = fmap

infixr 1 |<<

(>>|) ∷ ∀ a b f. (Functor f) ⇒ f a → (a → b) → f b
(>>|) = flip fmap

infixl 1 >>|

(|>) ∷ a → (a → b) → b
(|>) = (&)

infixl 1 |>

traverseM ∷
  (Monad m, Traversable m, Applicative f) ⇒
  (a1 → f (m a2)) →
  m a1 →
  f (m a2)
traverseM f = fmap join . traverse f

instance Show (a → b) where
  show _ = "fun"

newtype Symbol = Sym Text deriving (Eq, Hashable, Semigroup, Ord)

instance Show Symbol where
  show (Sym t) = T.unpack t

instance IsString Symbol where
  fromString = intern

intern ∷ String → Symbol
intern = Sym . T.pack

unintern ∷ Symbol → String
unintern (Sym s) = T.unpack s

unixTime ∷ IO Double
unixTime = fromRational . realToFrac |<< getPOSIXTime

newtype Flip p a b = Flip {runFlip ∷ p b a}
  deriving (Show, Generic, Eq, Ord, Typeable)

untilNothingNTimesM ∷ (Num t, Ord t, Enum t, Monad f) ⇒ f Bool → t → f ()
untilNothingNTimesM f n
  | n <= 0 = pure ()
  | otherwise = do
    f >>= \case
      True → untilNothingNTimesM f (pred n)
      False → pure ()

untilNothing ∷ (t → Maybe t) → t → t
untilNothing f a = case f a of
  Nothing → a
  Just a → untilNothing f a

-- | like sortOn from the stdlib, is an optimized version of `sortBy (comparing f)`
-- However instead of sorting from lowest to highest, this sorts from higher to lowest
sortOnFlip ∷ Ord b ⇒ (a → b) → [a] → [a]
sortOnFlip f =
  fmap snd . sortBy (flip (comparing fst))
    . fmap
      ( \x →
          let y = f x
           in y `seq` (y, x)
      )

uncurry3 ∷ (a → b → c → d) → (a, b, c) → d
uncurry3 fn (a, b, c) = fn a b c

curry3 ∷ ((a, b, c) → d) → a → b → c → d
curry3 fn a b c = fn (a, b, c)
