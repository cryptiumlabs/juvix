{-# LANGUAGE OverloadedLists #-}

module Juvix.Core.Common.Context.Traverse.Types
  ( -- * Output types
    Entry (..),
    Group,
    Group',
    Groups,
    Groups',
    Prefix,
    Deps,

    -- * Capabilities
    Env,
    PrefixReader,
    OutputState,
    DepsState,
    run,
    run_,

    -- * Operations
    addGroup,
    withPrefix,
    qualify,
    prefixM,
    applyPrefix,
    addDeps,
  )
where

import qualified Data.DList as D
import Juvix.Core.Common.Context.Types
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Core.Common.Context.Types as Context
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

-- | A definition identified by its fully-qualified name.
data Entry term ty sumRep
  = Entry
      { name :: NameSymbol.T,
        def :: Definition term ty sumRep
      }
  deriving (Eq, Show, Generic, Data)

-- | A recursive group of definitions.
type Group term ty sumRep = NonEmpty (Entry term ty sumRep)

type Group' term ty sumRep = D.DList (Entry term ty sumRep)

-- | All recursive groups in a context, in arbitrary order.
type Groups term ty sumRep =
  HashMap NameSymbol.Mod [Group term ty sumRep]

type Groups' term ty sumRep =
  HashMap NameSymbol.Mod (D.DList (Group term ty sumRep))

-- | Module name prefix
newtype Prefix = P (D.DList Symbol)

type Deps = HashMap NameSymbol.Mod (HashSet NameSymbol.Mod)

data S term ty sumRep
  = S
      { prefix :: Prefix,
        output :: Groups' term ty sumRep,
        curNameSpace :: Context.NameSpace term ty sumRep,
        deps :: Deps
      }
  deriving (Generic)

type Alias term ty sumRep = State (S term ty sumRep)

newtype Env term ty sumRep a = Env (Alias term ty sumRep a)
  deriving newtype (Functor, Applicative, Monad)
  deriving
    ( HasSource "prefix" Prefix,
      HasReader "prefix" Prefix
    )
    via ReaderField "prefix" (Alias term ty sumRep)
  deriving
    ( HasSource "output" (Groups' term ty sumRep),
      HasSink "output" (Groups' term ty sumRep),
      HasState "output" (Groups' term ty sumRep)
    )
    via StateField "output" (Alias term ty sumRep)
  deriving
    ( HasSource "curNameSpace" (Context.NameSpace term ty sumRep),
      HasReader "curNameSpace" (Context.NameSpace term ty sumRep)
    )
    via ReaderField "curNameSpace" (Alias term ty sumRep)
  deriving
    ( HasSource "deps" Deps,
      HasSink "deps" Deps,
      HasState "deps" Deps
    )
    via StateField "deps" (Alias term ty sumRep)

type PrefixReader = HasReader "prefix" Prefix

type OutputState term ty sumRep =
  HasState "output" (Groups' term ty sumRep)

type DepsState = HasState "deps" Deps

run_ :: Context.NameSpace term ty sumRep ->
        Env term ty sumRep a -> (Groups term ty sumRep, Deps)
run_ curns act =
  let (_, grps, deps) = run curns act in (grps, deps)

run :: Context.NameSpace term ty sumRep ->
       Env term ty sumRep a -> (a, Groups term ty sumRep, Deps)
run curNameSpace (Env act) =
  let (res, S {output, deps}) = runState act initState in
  (res, toList <$> output, deps)
  where
    initState = S {prefix, output = [], curNameSpace, deps = []}
    prefix = P [Context.topLevelName]

-- | Add a group to the final output.
addGroup :: (PrefixReader m, OutputState term ty sumRep m, Foldable t)
         => t (Entry term ty sumRep) -> m ()
addGroup grp = do
  prefix <- prefixM
  case nonEmpty $ toList grp of
    Just grp -> modify @"output" $ HashMap.alter f prefix
      where f = Just . maybe [grp] (<> [grp])
    Nothing -> pure ()

-- | Add dependencies on the given names to the current namespace.
addDeps :: (Foldable t, DepsState m, PrefixReader m) => t NameSymbol.T -> m ()
addDeps deps = do
  let mods = HashSet.fromList $ map NameSymbol.mod $ toList deps
  let f = Just . maybe mods (HashSet.union mods)
  prefix <- prefixM
  modify @"deps" $ HashMap.alter f prefix

toMod :: Prefix -> NameSymbol.Mod
toMod (P p) = toList p

prefixM :: PrefixReader m => m NameSymbol.Mod
prefixM = asks @"prefix" toMod


-- | Extend the current module prefix.
--
-- >>> 'fst' $ 'run' $ 'withPrefix' \"A\" $ 'qualify' \"X\"
-- A.X
-- >>> 'fst' $ 'run' $ 'withPrefix' \"A\" $ 'withPrefix' \"B\" $ 'qualify' \"X\"
-- A.B.X
withPrefix :: PrefixReader m => Symbol -> m a -> m a
withPrefix n = local @"prefix" \(P pfx) -> P $ D.snoc pfx n

-- | Qualify a name by the current module prefix.
qualify :: PrefixReader m => Symbol -> m NameSymbol.T
qualify n = asks @"prefix" \pfx -> applyPrefix pfx n

-- | Apply a prefix to a name.
applyPrefix :: Prefix -> Symbol -> NameSymbol.T
applyPrefix (P pfx) = NameSymbol.qualify1 pfx
