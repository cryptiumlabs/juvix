module Juvix.Utility.PrettyPrint where

import qualified Data.Text       as T
import           Protolude
import qualified Type.Reflection as R

class PrettyPrint a where
  prettyPrintValue ∷ a → Text

  default prettyPrintValue ∷ (Show a) ⇒ a → Text
  prettyPrintValue = show

  prettyPrintType ∷ a → Text

  default prettyPrintType ∷ (R.Typeable a) ⇒ a → Text
  prettyPrintType = show . R.typeOf

instance (PrettyPrint a, R.Typeable a) ⇒ PrettyPrint [a] where
  prettyPrintValue l = T.concat ["[", T.intercalate ", " (fmap prettyPrintValue l), "]"]

instance (PrettyPrint a, PrettyPrint b, R.Typeable a, R.Typeable b) ⇒ PrettyPrint (Either a b) where
  prettyPrintValue = \case
    Right r → "Right " <> prettyPrintValue r
    Left l  → "Left " <> prettyPrintValue l

instance (PrettyPrint a, PrettyPrint b, R.Typeable a, R.Typeable b) ⇒ PrettyPrint (a, b) where
  prettyPrintValue (a, b) = T.concat ["(", prettyPrintValue a, ", ", prettyPrintValue b, ")"]

instance (PrettyPrint a, R.Typeable a) ⇒ PrettyPrint (Maybe a) where
  prettyPrintValue (Just v) = T.concat ["Just ", prettyPrintValue v]
  prettyPrintValue Nothing  = "Nothing"

instance PrettyPrint ()
instance PrettyPrint Bool
instance PrettyPrint Int
instance PrettyPrint Integer
instance PrettyPrint Text

instance PrettyPrint (a → b) where
  prettyPrintValue  = const "λ"
  prettyPrintType   = const "λ"
