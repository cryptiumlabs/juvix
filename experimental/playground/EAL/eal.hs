{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

import Data.Map.Strict as Map
import qualified Data.Typeable as T
import Juvix.Library hiding (link, reduce)
import qualified Type.Reflection as R

data Eal a where
  Term :: SomeSymbol -> Eal SomeSymbol
  Lambda :: (Dynamical a, Dynamical b) => SomeSymbol -> Types a -> Term b -> Eal (a -> b)
  App :: Term (a -> b) -> Term a -> Eal b

deriving instance Show (Eal a)

data Term a = Bang Integer (Eal a)
  deriving (Show)

-- remove the Bang'' later... only a hack for now!
newtype Bang a = Bang'' a deriving (Show)

data UBang a

--deriving instance Show (Bang a)
deriving instance Show (UBang a)

deriving instance Typeable (Bang a)

deriving instance Typeable (Bang)

deriving instance Typeable (UBang a)

data Types a where
  Lolly :: Types b -> Types (b -> a)
  BangT :: Types a -> Types (Bang a)
  UBangT :: Types a -> Types (UBang a)
  TermT :: Types a
  Specific :: Types a

deriving instance Show (Types a)

data TypeErrors
  = MisMatchArguments
  | MissingOverUse

data WrappedTypes where
  WrapT :: (Arrowable a, Bangable a) => Types a -> WrappedTypes

deriving instance Show WrappedTypes

typeOf ::
  ( HasState "ctxt" (Map SomeSymbol WrappedTypes) m,
    HasThrow "typ" TypeErrors m,
    Arrowable a
  ) =>
  Eal a ->
  m WrappedTypes
typeOf (Term s) = do
  ctxt <- get @"ctxt"
  case ctxt Map.!? s of
    Nothing -> throw @"typ" MissingOverUse
    Just x@(WrapT (_ :: Types a)) ->
      case isBang (Proxy :: Proxy a) of
        True -> pure x
        False -> put @"ctxt" (Map.delete s ctxt) >> pure x
typeOf (Lambda sym (symType :: Types a) t) = do
  modify @"ctxt" (Map.insert sym (WrapT symType))
  WrapT (_ :: Types b) <- typeOfTerm t
  pure (WrapT (Lolly symType :: Types (a -> b)))
typeOf (App t1 t2) = do
  WrapT (_ :: Types c) <- typeOfTerm t1
  WrapT (_ :: Types b) <- typeOfTerm t2
  (ArrowType (_ :: Proxy from), ArrowType (_ :: Proxy to)) <- pure (isFromA (Proxy :: Proxy c))
  let ta = R.typeRep :: R.TypeRep b
      tb = R.typeRep :: R.TypeRep from
  case (R.eqTypeRep ta tb) of
    Nothing -> throw @"typ" MisMatchArguments
    Just T.HRefl ->
      pure (WrapT (Specific :: Types from))

--data Forget (a :: k) where
--  F :: (a :: k) -> Forget (a :: *)

--f :: forall k (a :: k). Arrowable a => Proxy a -> [Bool]
f p = do
  (ArrowType (_ :: Proxy k), ArrowType (_ :: Proxy to)) <- pure (isFromA p)
  let ta = R.typeRep :: R.TypeRep k
      tb = R.typeRep :: R.TypeRep Int
      tc = R.typeRep :: R.TypeRep to
      td = R.typeRep :: Arrowable to => R.TypeRep to
  case (R.eqTypeRep ta tb) of
    Nothing -> [False, False]
    Just T.HRefl ->
      case (R.eqTypeRep tc td) of
        Just T.HRefl -> [True]
        Nothing -> [False]

-- typeOfTerm :: (HasState "ctxt" (Map SomeSymbol WrappedTypes) m, HasThrow "typ" TypeErrors m
--          , Arrowable a)
--        ⇒ Term a → m WrappedTypes
typeOfTerm (Bang n t) = undefined

type family F a :: Bool where
  F (Bang _) = 'True
  F a = 'False

class (Typeable a) => Bangable a where
  isBang :: Proxy a -> Bool

instance (Typeable a, F a ~ flag, Bangable' flag a) => Bangable a where
  isBang = isBang' (Proxy :: Proxy flag)

class Bangable' (flag :: Bool) a where
  isBang' :: Proxy flag -> Proxy a -> Bool

instance Bangable' 'True (Bang a) where
  isBang' _ _ = True

instance Bangable' 'False a where
  isBang' _ _ = False

type family G a :: Bool where
  G (a -> _) = 'True
  G a = 'False

data ArrowType where
  ArrowType :: (Arrowable a) => Proxy a -> ArrowType

deriving instance Show ArrowType

class Bangable a => Arrowable a where
  isFromA :: Proxy a -> (ArrowType, ArrowType)

instance (Bangable a, G a ~ flag, Arrowable' flag a) => Arrowable a where
  isFromA = isFromA' (Proxy :: Proxy flag)

class Bangable a => Arrowable' flag a where
  isFromA' :: Proxy flag -> Proxy a -> (ArrowType, ArrowType)

instance (Arrowable a, Arrowable b) => Arrowable' 'True (a -> b) where
  isFromA' _ (Proxy :: Proxy (a -> b)) = (ArrowType (Proxy :: Proxy a), ArrowType (Proxy :: Proxy b))

instance (Arrowable a) => Arrowable' 'False a where
  isFromA' _ _ = (ArrowType (Proxy :: Proxy NotMatch), ArrowType (Proxy :: Proxy a))

data NotMatch

type Dynamical a = Arrowable a
