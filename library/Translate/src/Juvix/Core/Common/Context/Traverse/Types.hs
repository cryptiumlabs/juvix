{-# LANGUAGE OverloadedLists #-}

module Juvix.Core.Common.Context.Traverse.Types
  ( -- * Output types
    Entry (..),
    Group,
    Group',
    Groups,
    Groups',
    Prefix (..),
    Deps,

    -- * Capabilities
    Env,
    ContextReader,
    OutputState,
    DepsState,
    HasRecGroups,
    run,
    run_,
  )
where

import qualified Data.DList as D
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Juvix.Core.Common.Context.Types
import qualified Juvix.Core.Common.Context.Types as Context
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol

-- | A definition identified by its fully-qualified name.
data Entry term ty sumRep
  = Entry
      { name :: NameSymbol.T,
        def :: Definition term ty sumRep
      }
  deriving (Eq, Show, Generic)

-- | A recursive group of definitions, in an arbitrary order.
type Group term ty sumRep = NonEmpty (Entry term ty sumRep)

type Group' term ty sumRep = D.DList (Entry term ty sumRep)

-- | All recursive groups in a context. Each namespace has its groups in
-- dependency order.
type Groups term ty sumRep =
  HashMap NameSymbol.Mod [Group term ty sumRep]

type Groups' term ty sumRep =
  HashMap NameSymbol.Mod (D.DList (Group term ty sumRep))

-- | Module name prefix
newtype Prefix = P (D.DList Symbol)

type Deps = HashMap NameSymbol.Mod (HashSet NameSymbol.Mod)

data S term ty sumRep
  = S
      { output :: Groups' term ty sumRep,
        context :: Context.T term ty sumRep,
        deps :: Deps
      }
  deriving (Generic)

type Alias term ty sumRep = State (S term ty sumRep)

newtype Env term ty sumRep a = Env (Alias term ty sumRep a)
  deriving newtype (Functor, Applicative, Monad)
  deriving
    ( HasSource "output" (Groups' term ty sumRep),
      HasSink "output" (Groups' term ty sumRep),
      HasState "output" (Groups' term ty sumRep)
    )
    via StateField "output" (Alias term ty sumRep)
  deriving
    ( HasSource "context" (Context.T term ty sumRep),
      HasSink "context" (Context.T term ty sumRep),
      HasState "context" (Context.T term ty sumRep)
    )
    via StateField "context" (Alias term ty sumRep)
  deriving
    ( HasSource "deps" Deps,
      HasSink "deps" Deps,
      HasState "deps" Deps
    )
    via StateField "deps" (Alias term ty sumRep)

type ContextReader term ty sumRep =
  HasState "context" (Context.T term ty sumRep)

type OutputState term ty sumRep =
  HasState "output" (Groups' term ty sumRep)

type DepsState = HasState "deps" Deps

type HasRecGroups term ty sumRep m =
  ( ContextReader term ty sumRep m,
    DepsState m,
    OutputState term ty sumRep m,
    Data term,
    Data ty,
    Data sumRep
  )

run_ ::
  Context.T term ty sumRep ->
  Env term ty sumRep a ->
  (Groups term ty sumRep, Deps)
run_ context act =
  let (_, grps, deps) = run context act in (grps, deps)

run ::
  Context.T term ty sumRep ->
  Env term ty sumRep a ->
  (a, Groups term ty sumRep, Deps)
run context (Env act) =
  let (res, S {output, deps}) = runState act initState
   in (res, toList <$> output, deps)
  where
    initState = S {output = [], context, deps = []}
