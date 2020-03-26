{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

import Juvix.Library hiding (link, reduce)

newtype Bang a = Bang'' a deriving (Show)

type family F a ∷ Bool where
  F (Bang _) = 'True
  F a = 'False

class (Typeable a) ⇒ Bangable a where
  isBang ∷ Proxy a → Bool

instance (Typeable a, F a ~ flag, Bangable' flag a) ⇒ Bangable a where
  isBang = isBang' (Proxy ∷ Proxy flag)

class Bangable' (flag ∷ Bool) a where
  isBang' ∷ Proxy flag → Proxy a → Bool

instance Bangable' 'True (Bang a) where
  isBang' _ _ = True

instance Bangable' 'False a where
  isBang' _ _ = False

type family G a ∷ Bool where
  G (a → _) = 'True
  G a = 'False

--data ArrowType where
--  ArrowType ∷ (Arrowable a) ⇒ Proxy a → ArrowType

-- deriving instance Show (ArrowType)

class Bangable a ⇒ Arrowable a where
  isFromA ∷ Proxy a → (Proxy b, Proxy c)

instance (Bangable a, G a ~ flag, Arrowable' flag a) ⇒ Arrowable a where
  isFromA = isFromA' (Proxy ∷ Proxy flag)

class Bangable a ⇒ Arrowable' flag a where
  isFromA' ∷ Proxy flag → Proxy a → (Proxy b, Proxy c)

instance (Arrowable a, Arrowable b) ⇒ Arrowable' 'True (a → b) where
  isFromA' _ (Proxy ∷ Proxy (a → b)) = ((Proxy ∷ Proxy a), (Proxy ∷ Proxy b))

instance (Arrowable a) ⇒ Arrowable' 'False a where
  isFromA' _ _ = ((Proxy ∷ Proxy NotMatch), (Proxy ∷ Proxy a))

data NotMatch

type Dynamical a = Arrowable a
