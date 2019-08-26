{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

import           Juvix.Library hiding (link, reduce)

type family (G a b) :: Bool where
  G (a → _) a = 'True
  G a       _ = 'False

class (Typeable a, Typeable b) ⇒ Dynamical a b where
  isFromA ∷ Proxy a → Proxy b → Bool

instance (Typeable b, Typeable a, G a b ~ flag, Dynamical' flag a b) ⇒ Dynamical a b where
  isFromA = isFromA' (Proxy :: Proxy flag)

class Dynamical' flag a b where
  isFromA' ∷ Proxy flag → Proxy a → Proxy b → Bool

instance Dynamical' 'True (a → b) a where
  isFromA' _ (Proxy :: Proxy (a → b)) (Proxy :: Proxy a) = True

instance Dynamical' 'False a b where
  isFromA' _ _ _ = False
