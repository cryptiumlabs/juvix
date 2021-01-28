{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Types to support partial application and polymorphic primitives.
module Juvix.Core.Application where

import Data.Bifoldable
import Data.Bitraversable
import qualified Juvix.Core.IR.Types as IR
import Juvix.Library
import qualified Juvix.Library.Usage as Usage

-- |
-- A primitive along with its type, and possibly some arguments.
data Return' ext ty term
  = -- | Partially applied primitive holding the arguments already given
    Cont
      { -- | head of application
        fun :: Take ty term,
        -- | arguments
        args :: [Arg' ext ty term],
        -- | number of arguments still expected
        numLeft :: Natural
      }
  | -- | A primitive with no arguments
    Return
      { retType :: ty,
        retTerm :: term
      }
  deriving (Generic, Functor, Foldable, Traversable)

deriving instance
  (Show (ParamVar ext), Show ty, Show term) =>
  Show (Return' ext ty term)

deriving instance
  (Eq (ParamVar ext), Eq ty, Eq term) =>
  Eq (Return' ext ty term)

instance Bifunctor (Return' ext) where
  bimap = bimapDefault

instance Bifoldable (Return' ext) where
  bifoldMap = bifoldMapDefault

instance Bitraversable (Return' ext) where
  bitraverse f g = \case
    Cont s ts n ->
      Cont <$> bitraverse f g s
        <*> traverse (bitraverse f (traverse g)) ts
        <*> pure n
    Return a s ->
      Return <$> f a <*> g s

type Return = Return' IR.NoExt

-- | The representation of variables used in IR.Term' ext
type family ParamVar ext :: Type

data DeBruijn
  = BoundVar IR.BoundVar
  | FreeVar IR.GlobalName
  deriving (Show, Eq, Generic)

type instance ParamVar IR.NoExt = DeBruijn

data ArgBody' ext term
  = VarArg (ParamVar ext)
  | TermArg term
  deriving (Generic, Functor, Foldable, Traversable)

pattern BoundArg ::
  (ParamVar ext ~ DeBruijn) => IR.BoundVar -> ArgBody' ext term
pattern BoundArg i = VarArg (BoundVar i)

pattern FreeArg ::
  (ParamVar ext ~ DeBruijn) => IR.GlobalName -> ArgBody' ext term
pattern FreeArg x = VarArg (FreeVar x)

deriving instance (Show (ParamVar ext), Show term) => Show (ArgBody' ext term)

deriving instance (Eq (ParamVar ext), Eq term) => Eq (ArgBody' ext term)

type ArgBody = ArgBody' IR.NoExt

type Arg' ext ty term = Take ty (ArgBody' ext term)

type Arg ty term = Arg' IR.NoExt ty term

argToBase :: Alternative f => ArgBody' ext term -> f term
argToBase (TermArg t) = pure t
argToBase _ = empty

-- |
-- An argument to a partially applied primitive, which must be
-- fully-applied itself.
data Take ty term
  = Take
      { usage :: Usage.T,
        type' :: ty,
        term :: term
      }
  deriving (Show, Eq, Generic, Functor, Foldable, Traversable)

instance Bifunctor Take where
  bimap = bimapDefault

instance Bifoldable Take where
  bifoldMap = bifoldMapDefault

instance Bitraversable Take where
  bitraverse f g (Take π a s) = Take π <$> f a <*> g s

takeToReturn :: Take ty term -> Return' ext ty term
takeToReturn (Take {type', term}) = Return {retType = type', retTerm = term}
