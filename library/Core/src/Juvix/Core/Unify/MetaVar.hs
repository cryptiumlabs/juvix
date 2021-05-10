{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Juvix.Core.Unify.MetaVar
  ( MetaVar,
    metaVars,

    -- * Sets
    MetaSet,
    toListS,
    fromListS,
    singleS,
    memberS,
    insertS,
    filterS,
    nullS,

    -- * Maps
    MetaMap,
    toListM,
    fromListM,
    singleM,
    memberM,
    lookupM,
    insertM,
    nullM,
  )
where

import Data.Coerce
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import GHC.Exts (IsList (..))
import Juvix.Core.IR.Evaluator.Weak
import Juvix.Library

newtype MetaVar = MV Int
  deriving newtype (Eq, Ord, Show, Read, Hashable)

instance HasWeak MetaVar where weakBy' _ _ α = α

metaVars :: [MetaVar]
metaVars = MV <$> [0 ..]

newtype MetaSet = MS IntSet
  deriving newtype (Eq, Ord, Show, Monoid, Semigroup)

instance IsList MetaSet where
  type Item MetaSet = MetaVar
  toList = toListS
  fromList = fromListS

instance HasWeak MetaSet where weakBy' _ _ αs = αs

toListS :: MetaSet -> [MetaVar]
toListS = coerce IntSet.toList

fromListS :: Foldable t => t MetaVar -> MetaSet
fromListS = foldl' (flip insertS) mempty
{-# SPECIALIZE fromListS :: [MetaVar] -> MetaSet #-}

singleS :: MetaVar -> MetaSet
singleS α = [α]

memberS :: MetaVar -> MetaSet -> Bool
memberS = coerce IntSet.member

insertS :: MetaVar -> MetaSet -> MetaSet
insertS = coerce IntSet.insert

filterS :: (MetaVar -> Bool) -> MetaSet -> MetaSet
filterS = coerce IntSet.filter

nullS :: MetaSet -> Bool
nullS = coerce IntSet.null

newtype MetaMap a = MM (IntMap a)
  deriving newtype (Eq, Ord, Show, Monoid, Semigroup)
  deriving stock (Functor, Foldable, Traversable)

instance HasWeak a => HasWeak (MetaMap a) where
  weakBy' b i (MM δ) = MM $ IntMap.map (weakBy' b i) δ

instance IsList (MetaMap a) where
  type Item (MetaMap a) = (MetaVar, a)
  toList = toListM
  fromList = fromListM

toListM :: forall a. MetaMap a -> [(MetaVar, a)]
toListM = coerce $ IntMap.toList @a

fromListM :: Foldable t => t (MetaVar, a) -> MetaMap a
fromListM = foldl' (\δ (α, x) -> insertM α x δ) mempty
{-# SPECIALIZE fromListM :: [(MetaVar, a)] -> MetaMap a #-}

singleM :: MetaVar -> a -> MetaMap a
singleM α x = [(α, x)]

memberM :: forall a. MetaVar -> MetaMap a -> Bool
memberM = coerce $ IntMap.member @a

lookupM :: forall a. MetaVar -> MetaMap a -> Maybe a
lookupM = coerce $ IntMap.lookup @a

insertM :: forall a. MetaVar -> a -> MetaMap a -> MetaMap a
insertM = coerce $ IntMap.insert @a

nullM :: forall a. MetaMap a -> Bool
nullM = coerce $ IntMap.null @a
