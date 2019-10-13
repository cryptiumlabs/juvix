module Juvix.Utility.Helper where

import           Juvix.Library

newtype Flip p a b = Flip { runFlip ∷ p b a }
                   deriving (Show, Generic, Eq, Ord, Typeable)


untilNothingNTimesM ∷ (Num t, Ord t, Enum t, Monad f) ⇒ f Bool → t → f ()
untilNothingNTimesM f n
  | n <= 0  = pure ()
  | otherwise = do
      f >>= \case
        True → untilNothingNTimesM f (pred n)
        False → pure ()

untilNothing ∷ (t → Maybe t) → t → t
untilNothing f a = case f a of
  Nothing → a
  Just a  → untilNothing f a


-- | like sortOn from the stdlib, is an optimized version of `sortBy (comparing f)`
-- However instead of sorting from lowest to highest, this sorts from higher to lowest
sortOnFlip ∷ Ord b ⇒ (a → b) → [a] → [a]
sortOnFlip f =
  fmap snd . sortBy (flip (comparing fst)) . fmap (\x → let y = f x in y `seq` (y, x))
