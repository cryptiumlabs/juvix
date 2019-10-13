module Juvix.Utility.Dynamical where

import           Data.Typeable
import           Protolude
import qualified Type.Reflection           as R

import           Juvix.Utility.PrettyPrint
import           Juvix.Utility.Sugar

{-
 - This is a modified version of Data.Dynamic and friends.
 - I think generics might be a better approach...
 -}

class (Eq a, PrettyPrint a, R.Typeable a) ⇒ Dynamical a where
  unSum     :: forall m . (MonadError DynamicError m) ⇒ Proxy a → m (DynamicType, DynamicType)
  unSum     = throw . NotASumType

  unProduct :: forall m . (MonadError DynamicError m) ⇒ Proxy a → m (DynamicType, DynamicType)
  unProduct = throw . NotAProductType

  unOption  :: forall m . (MonadError DynamicError m) ⇒ Proxy a → m DynamicType
  unOption  = throw . NotAnOptionType

  unArrow :: forall m . (MonadError DynamicError m) ⇒ Proxy a → m (DynamicType, DynamicType)
  unArrow   = throw . NotAnArrowType

data DynamicError where
  CannotCast      :: forall a . Dynamical a ⇒ DynamicValue → R.TypeRep a → DynamicError
  CannotUnify     :: forall a b . (Dynamical a, Dynamical b) ⇒ Proxy a → Proxy b → DynamicError
  NotAnOptionType :: forall a . Dynamical a ⇒ Proxy a → DynamicError
  NotAProductType :: forall a . Dynamical a ⇒ Proxy a → DynamicError
  NotASumType     :: forall a . Dynamical a ⇒ Proxy a → DynamicError
  NotAnArrowType  :: forall a . Dynamical a ⇒ Proxy a → DynamicError

data DynamicValue where
  DynamicValue :: forall a . Dynamical a ⇒ a → DynamicValue

data DynamicType where
  DynamicType :: forall a . (Dynamical a) ⇒ Proxy a → DynamicType

instance Eq DynamicType where
  (==) (DynamicType (Proxy :: Proxy x)) (DynamicType (Proxy :: Proxy y)) =
    case eqT :: Maybe (x :~: y) of
      Just Refl -> True
      _         -> False

instance PrettyPrint DynamicType where
  prettyPrintValue v = prettyPrintType v

  prettyPrintType (DynamicType (Proxy :: Proxy a)) = prettyPrintType (undefined :: a)

toDynamicType :: forall a . (Dynamical a) ⇒ Proxy a → DynamicType
toDynamicType = DynamicType

toDynamicValue :: forall a. (Dynamical a) ⇒ a → DynamicValue
toDynamicValue = DynamicValue

fromDynamicValue :: forall a m . (MonadError DynamicError m, Dynamical a) ⇒ DynamicValue → m a
fromDynamicValue dyn@(DynamicValue (b :: bT)) = do
  let castRep = R.typeRep :: R.TypeRep a
  case R.eqTypeRep castRep (R.typeRep :: R.TypeRep bT) of
    Just R.HRefl -> return b
    Nothing      -> throw (CannotCast dyn castRep)

liftDyn1 :: forall b . (forall a . Dynamical a ⇒ a → b) → DynamicValue → b
liftDyn1 func (DynamicValue a) = func a

appDyn1 :: forall a b m . (Dynamical a, MonadError DynamicError m) ⇒ (a → b) → DynamicValue → m b
appDyn1 func = (|<<) func . fromDynamicValue

instance Dynamical ()
instance Dynamical Text
instance Dynamical Bool
instance Dynamical Integer

instance (Dynamical a, Dynamical b) ⇒ Dynamical (Either a b) where
  unSum (Proxy :: Proxy (Either a b)) = return (DynamicType (Proxy :: Proxy a), DynamicType (Proxy :: Proxy b))

instance (Dynamical a, Dynamical b) ⇒ Dynamical (a, b) where
  unProduct (Proxy :: Proxy (x, y)) = return (DynamicType (Proxy :: Proxy a), DynamicType (Proxy :: Proxy b))

instance (Dynamical a) ⇒ Dynamical (Maybe a) where
  unOption (Proxy :: Proxy (Maybe a)) = return (DynamicType (Proxy :: Proxy a))

instance (Eq (a → b)) where
  _ == _ = False

instance (Dynamical a, Dynamical b) ⇒ Dynamical (a → b) where
  unArrow (Proxy :: Proxy (a → b)) = return (DynamicType (Proxy :: Proxy a), DynamicType (Proxy :: Proxy b))
