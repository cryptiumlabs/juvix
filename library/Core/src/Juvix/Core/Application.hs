{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Types to support partial application and polymorphic primitives.
module Juvix.Core.Application
  ( Return' (..),
    DeBruijn (..),
    IsParamVar (..),
    Arg' (..),
    Arg,
    pattern BoundArg,
    pattern FreeArg,
    Take (..),
    argToTake,
    takeToReturn,
  )
where

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
        <*> traverse (bitraverse f g) ts
        <*> pure n
    Return a s ->
      Return <$> f a <*> g s

type Return = Return' IR.NoExt

-- | The representation of variables used in IR.Term' ext
class IsParamVar ext where
  type ParamVar ext :: Type

  freeVar :: Proxy ext -> IR.GlobalName -> Maybe (ParamVar ext)
  boundVar :: Proxy ext -> IR.BoundVar -> Maybe (ParamVar ext)

data DeBruijn
  = BoundVar IR.BoundVar
  | FreeVar IR.GlobalName
  deriving (Show, Eq, Generic)

instance IsParamVar IR.NoExt where
  type ParamVar IR.NoExt = DeBruijn
  freeVar _ = Just . FreeVar
  boundVar _ = Just . BoundVar

data Arg' ext ty term
  = VarArg (ParamVar ext)
  | TermArg (Take ty term)
  deriving (Generic, Functor, Foldable, Traversable)

type Arg = Arg' IR.NoExt

pattern BoundArg ::
  (ParamVar ext ~ DeBruijn) => IR.BoundVar -> Arg' ext ty term
pattern BoundArg i = VarArg (BoundVar i)

pattern FreeArg ::
  (ParamVar ext ~ DeBruijn) => IR.GlobalName -> Arg' ext ty term
pattern FreeArg x = VarArg (FreeVar x)

{-# COMPLETE TermArg, BoundArg, FreeArg #-}

deriving instance
  (Show (ParamVar ext), Show ty, Show term) =>
  Show (Arg' ext ty term)

deriving instance
  (Eq (ParamVar ext), Eq ty, Eq term) =>
  Eq (Arg' ext ty term)

instance Bifunctor (Arg' ext) where bimap = bimapDefault

instance Bifoldable (Arg' ext) where bifoldMap = bifoldMapDefault

instance Bitraversable (Arg' ext) where
  bitraverse _ _ (VarArg x) = pure $ VarArg x
  bitraverse f g (TermArg t) = TermArg <$> bitraverse f g t

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

argToTake :: Alternative f => Arg' ext ty term -> f (Take ty term)
argToTake (TermArg t) = pure t
argToTake _ = empty

argToReturn :: Alternative f => Arg' ext ty term -> f (Return' ext' ty term)
argToReturn = fmap takeToReturn . argToTake

takeToReturn :: Take ty term -> Return' ext ty term
takeToReturn (Take {type', term}) = Return {retType = type', retTerm = term}
