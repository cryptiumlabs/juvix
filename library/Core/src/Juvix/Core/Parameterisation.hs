{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE UndecidableInstances #-}

module Juvix.Core.Parameterisation
  ( Parameterisation (..),
    Builtins,
    PrimType,
    TypedPrim,
    TypedPrim',
    ApplyError,
    ApplyError' (..),
    CanApply (..),
    apply1,
    mapApplyErr,
  )
where

import qualified Juvix.Core.Application as App
import Juvix.Core.IR.Types (BoundVar, GlobalName, NoExt)
import Juvix.Library
import Juvix.Library.HashMap (HashMap)
import qualified Juvix.Library.NameSymbol as NameSymbol

-- | @[A, B, ..., Z]@ represents the type
-- @π A -> ρ B -> ... -> Z@ for any usages @π@, @ρ@
type PrimType primTy = NonEmpty primTy

type Builtins p = HashMap NameSymbol.T p

data Parameterisation primTy primVal
  = Parameterisation
      { hasType :: primVal -> PrimType primTy -> Bool,
        builtinTypes :: Builtins primTy,
        builtinValues :: Builtins primVal,
        stringVal :: Text -> Maybe primVal,
        intVal :: Integer -> Maybe primVal,
        floatVal :: Double -> Maybe primVal
      }
  deriving (Generic)

data ApplyError' e a
  = ExtraArguments a (NonEmpty (Arg a))
  | InvalidArguments a (NonEmpty (Arg a))
  | Extra e

deriving instance (Eq e, Eq a, Eq (Arg a)) => Eq (ApplyError' e a)

deriving instance (Show e, Show a, Show (Arg a)) => Show (ApplyError' e a)

type ApplyError a = ApplyError' (ApplyErrorExtra a) a

class CanApply a where
  type ApplyErrorExtra a
  type ApplyErrorExtra a = Void

  type Arg a
  type Arg a = a

  pureArg :: a -> Maybe (Arg a)
  default pureArg :: (Arg a ~ a) => a -> Maybe (Arg a)
  pureArg = Just

  freeArg :: Proxy a -> GlobalName -> Maybe (Arg a)
  freeArg _ _ = Nothing

  boundArg :: Proxy a -> BoundVar -> Maybe (Arg a)
  boundArg _ _ = Nothing

  arity :: a -> Natural
  apply :: a -> NonEmpty (Arg a) -> Either (ApplyError a) a

mapApplyErr ::
  ( ApplyErrorExtra a ~ ApplyErrorExtra b,
    Arg a ~ a,
    Arg b ~ b
  ) =>
  (a -> b) ->
  Either (ApplyError a) a ->
  Either (ApplyError b) b
mapApplyErr wrap = bimap wrap' wrap
  where
    wrap' (ExtraArguments f xs) =
      ExtraArguments (wrap f) (map wrap xs)
    wrap' (InvalidArguments f xs) =
      InvalidArguments (wrap f) (map wrap xs)
    wrap' (Extra e) = Extra e

applyMaybe :: CanApply a => a -> NonEmpty (Arg a) -> Maybe a
applyMaybe f xs = either (const Nothing) Just $ apply f xs

apply1 :: CanApply a => a -> Arg a -> Either (ApplyError a) a
apply1 f x = apply f (x :| [])

apply1Maybe :: CanApply a => a -> Arg a -> Maybe a
apply1Maybe f x = applyMaybe f (x :| [])

type TypedPrim' ext ty val = App.Return' ext (PrimType ty) val

type TypedPrim ty val = TypedPrim' NoExt ty val
