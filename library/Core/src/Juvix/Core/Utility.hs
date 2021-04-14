{-# LANGUAGE BangPatterns #-}

module Juvix.Core.Utility
  ( -- * Capabilities
    HasNameStack,
    HasNameSupply,
    HasNames,
    HasNextPatVar,
    HasSymToPat,
    HasPatToSym,

    -- * Operations

    -- ** Symbols
    withName,
    lookupName,
    lookupIndex,
    withFresh,
    nextFresh,
    withNextPatVar,
    nextPatVar,

    -- ** Pattern variables
    getSymToPat,
    setSymToPat,
    getPatToSym,
    setPatToSym,

    -- * Infinite streams
    Stream (..),
    app,
    filter,
    take,
    names,
    patVarsExcept,
  )
where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap.Strict as PM
import qualified Data.IntSet as PS
import Data.List (findIndex, (!!))
import Juvix.Core.IR.Types (PatternMap, PatternSet, PatternVar)
import Juvix.Library hiding (filter, take)
import qualified Juvix.Library.NameSymbol as NameSymbol

type HasNameStack = HasReader "nameStack" [NameSymbol.T]

type HasNameSupply = HasState "nameSupply" (Stream NameSymbol.T)

type HasNames m = (HasNameStack m, HasNameSupply m)

type HasNextPatVar = HasState "nextPatVar" PatternVar

type HasSymToPat = HasState "symToPat" (HashMap NameSymbol.T PatternVar)

type HasPatToSym = HasState "patToSym" (PatternMap NameSymbol.T)

withName :: HasNameStack m => NameSymbol.T -> m a -> m a
withName name = local @"nameStack" (name :)

lookupName :: HasNameStack m => NameSymbol.T -> m (Maybe Int)
lookupName name = findIndex (== name) <$> ask @"nameStack"

lookupIndex :: HasNameStack m => Int -> m NameSymbol.T
lookupIndex ind = asks @"nameStack" (!! ind)

withFresh :: HasNames m => (NameSymbol.T -> m a) -> m a
withFresh act = do
  sym <- nextFresh
  local @"nameStack" (sym :) $ act sym

nextFresh :: HasNameSupply m => m NameSymbol.T
nextFresh = state @"nameSupply" \(x :> xs) -> (x, xs)

getSymToPat :: HasSymToPat m => NameSymbol.T -> m (Maybe PatternVar)
getSymToPat k = gets @"symToPat" $ HM.lookup k

setSymToPat :: HasSymToPat m => NameSymbol.T -> PatternVar -> m ()
setSymToPat k v = modify @"symToPat" $ HM.insert k v

getPatToSym :: HasPatToSym m => PatternVar -> m (Maybe NameSymbol.T)
getPatToSym k = gets @"patToSym" $ PM.lookup k

setPatToSym :: HasPatToSym m => PatternVar -> NameSymbol.T -> m ()
setPatToSym k v = modify @"patToSym" $ PM.insert k v

withNextPatVar :: HasNextPatVar m => (PatternVar -> m a) -> m a
withNextPatVar act = act =<< nextPatVar

nextPatVar :: HasNextPatVar m => m PatternVar
nextPatVar = state @"nextPatVar" \v -> (v, succ v)

-- | necessarily-infinite stream
data Stream a = (:>) {head :: a, tail :: Stream a}

-- | add a list to the front of a stream
app :: [a] -> Stream a -> Stream a
app l s = foldr (:>) s l

filter :: (a -> Bool) -> Stream a -> Stream a
filter p (x :> xs) = if p x then x :> xs' else xs' where xs' = filter p xs

take :: Natural -> Stream a -> [a]
take 0 _ = []
take n (x :> xs) = x : take (n - 1) xs

-- | Infinite stream of names @a, b, ..., z, a1, ..., z1, a2, ...@
names :: Stream NameSymbol.T
names = map (\c -> makeNS [c]) az `app` go (1 :: Natural)
  where
    az = ['a' .. 'z']
    go i = [makeNS $ a : show i | a <- az] `app` go (i + 1)
    makeNS x = NameSymbol.fromString x

patVarsExcept :: PatternSet -> Stream PatternVar
patVarsExcept vars = go (PS.minView vars) 0
  where
    go Nothing i = allFrom i
    go (Just (v, vs)) i = case compare v i of
      LT -> i :> go (PS.minView vs) (i + 1)
      EQ -> go (PS.minView vs) (i + 1)
      GT -> i :> go (Just (v, vs)) (i + 1)
    allFrom !i = i :> allFrom (i + 1)
