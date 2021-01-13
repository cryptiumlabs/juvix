{-# LANGUAGE DeriveTraversable #-}
-- |
-- Types to support partial application and polymorphic primitives.
module Juvix.Core.Application where

import Juvix.Library
import qualified Juvix.Library.Usage as Usage
import qualified Juvix.Core.IR.Types as IR
import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable

-- |
-- A primitive along with its type, and possibly some arguments.
data Return ty term
  = -- | Partially applied primitive holding the arguments already given
    Cont
      { -- | head of application
        fun :: Take ty term,
        -- | arguments
        args :: [Arg ty term],
        -- | number of arguments still expected
        numLeft :: Natural
      }
  | -- | A primitive with no arguments
    Return
      { retType :: ty,
        retTerm :: term
      }
  deriving (Show, Eq, Generic, Functor, Foldable, Traversable)

instance Bifunctor Return where
  bimap = bimapDefault

instance Bifoldable Return where
  bifoldMap = bifoldMapDefault

instance Bitraversable Return where
  bitraverse f g = \case
    Cont s ts n ->
      Cont <$> bitraverse f g s
           <*> traverse (bitraverse f (traverse g)) ts
           <*> pure n
    Return a s ->
      Return <$> f a <*> g s


data Arg' term
  = BoundArg IR.BoundVar
  | FreeArg  IR.GlobalName
  | TermArg  term
  deriving (Show, Eq, Generic, Functor, Foldable, Traversable)

argToTerm :: Alternative f => Arg' term -> f term
argToTerm (TermArg t) = pure t
argToTerm _ = empty

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

type Arg ty term = Take ty (Arg' term)
