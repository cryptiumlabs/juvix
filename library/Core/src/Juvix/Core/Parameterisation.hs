{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Juvix parameterises the type theory & core language over a set of
-- primitive data types and primitive values, which can include native data
-- types such as strings, integers, or sets, and native functions such as
-- addition, subtraction, string concatenation, set membership, etc. The
-- language & typechecker can then be instantiated over a particular backend
-- which provides concrete sets of primitives and a primitive type-checking
-- relation.
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
    apply1Maybe,
    applyMaybe,
    mapApplyErr,
    check3Equal,
    check2Equal,
    checkFirst2AndLast,
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

-- | A HashMap of builtins with their name.
type Builtins p = HashMap NameSymbol.T p

-- | Abstraction over backend specifics.
data Parameterisation primTy primVal = Parameterisation
  { -- | Check if a value is of a given type.
    hasType :: primVal -> PrimType primTy -> Bool,
    -- | Set of builtin types.
    builtinTypes :: Builtins primTy,
    -- | Set of builtin values.
    builtinValues :: Builtins primVal,
    -- | Build a value from a string.
    stringVal :: Text -> Maybe primVal,
    -- | Build a value from an integer.
    intVal :: Integer -> Maybe primVal,
    -- | Build a value from a float.
    floatVal :: Double -> Maybe primVal
  }
  deriving (Generic)

-- | Datatype representing different kinds of errors.
data ApplyError' e a
  = ExtraArguments a (NonEmpty (Arg a))
  | InvalidArguments a (NonEmpty (Arg a))
  | Extra e

deriving instance (Eq e, Eq a, Eq (Arg a)) => Eq (ApplyError' e a)

deriving instance (Show e, Show a, Show (Arg a)) => Show (ApplyError' e a)

-- | Just like 'ApplyError'', but with a default for the extra argument.
type ApplyError a = ApplyError' (ApplyErrorExtra a) a

-- | Class that implements application for its argument.
class CanApply a where
  -- | Type to represent extra errors that might occur during application.
  type ApplyErrorExtra a

  -- | Default extra errors.
  type ApplyErrorExtra a = Void

  -- | Type to use for arguments.
  type Arg a

  -- | Default to wrap 'a' into an 'Arg a'.
  type Arg a = a

  -- | Lift a value into an argument.
  pureArg :: a -> Maybe (Arg a)
  default pureArg :: (Arg a ~ a) => a -> Maybe (Arg a)
  pureArg = Just

  -- | Create a reference to a free variable.
  freeArg :: Proxy a -> GlobalName -> Maybe (Arg a)
  freeArg _ _ = Nothing

  -- | Create a reference to a bound variable.
  boundArg :: Proxy a -> BoundVar -> Maybe (Arg a)
  boundArg _ _ = Nothing

  -- | Arity of the function.
  arity :: a -> Natural

  -- | Apply using a non-empty list of arguments
  apply :: a -> NonEmpty (Arg a) -> Either (ApplyError a) a

-- | Apply a function 'wrap' over the result of an application.
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

-- | Version of 'apply' returning a Maybe instead of an Either.
applyMaybe :: CanApply a => a -> NonEmpty (Arg a) -> Maybe a
applyMaybe f xs = either (const Nothing) Just $ apply f xs

-- | Version of 'apply' that requires at least one argument.
apply1 :: CanApply a => a -> Arg a -> Either (ApplyError a) a
apply1 f x = apply f (x :| [])

-- | Version of 'apply1' that requires at returns a Maybe.
apply1Maybe :: CanApply a => a -> Arg a -> Maybe a
apply1Maybe f x = applyMaybe f (x :| [])

-- | A typed primitive.
type TypedPrim' ext ty val = App.Return' ext (PrimType ty) val

-- | A typed primitive.
type TypedPrim ty val = TypedPrim' NoExt ty val

check3Equal :: Eq a => NonEmpty a -> Bool
check3Equal (x :| [y, z])
  | x == y && x == z = True
  | otherwise = False
check3Equal (_ :| _) = False

check2Equal :: Eq a => NonEmpty a -> Bool
check2Equal (x :| [y])
  | x == y = True
  | otherwise = False
check2Equal (_ :| _) = False

checkFirst2AndLast :: Eq t => NonEmpty t -> (t -> Bool) -> Bool
checkFirst2AndLast (x :| [y, last]) check
  | check2Equal (x :| [y]) && check last = True
  | otherwise = False
checkFirst2AndLast (_ :| _) _ = False
