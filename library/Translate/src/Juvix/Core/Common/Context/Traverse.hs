{-# LANGUAGE OverloadedLists #-}

-- | Calculate mutually-recursive groups of definitions.
module Juvix.Core.Common.Context.Traverse
  ( traverseContext,
    traverseContext1,
    traverseContext_,
    traverseContext1_,
    Entry (..),
    Group,
    Groups,
    recGroups,
  )
where

import qualified Data.Graph as Graph
import qualified Data.HashSet as HashSet
import qualified Generics.SYB as SYB
import Juvix.Core.Common.Context.Traverse.Types
import qualified Juvix.Core.Common.Context.Types as Context
import qualified Juvix.Core.Common.NameSpace as NameSpace
import qualified Juvix.FrontendContextualise.InfixPrecedence.FreeVars as FV
import Juvix.Library
import qualified Juvix.Library.HashMap as HashMap
import qualified Juvix.Library.NameSymbol as NameSymbol

-- | Traverses a whole context by performing an action on each recursive group.
-- The groups are passed in dependency order but the order of elements within
-- each group is arbitrary.
traverseContext ::
  (Applicative f, Monoid t, Data a, Data b, Data c) =>
  -- | process one recursive group
  (Group a b c -> f t) ->
  Context.T a b c ->
  f t
traverseContext f = foldMapA f . recGroups

-- | As 'traverseContext' but ignoring the return value.
traverseContext_ ::
  (Applicative f, Data a, Data b, Data c) =>
  -- | process one recursive group
  (Group a b c -> f z) ->
  Context.T a b c ->
  f ()
traverseContext_ f = traverse_ f . recGroups

-- | Same as 'traverseContext', but the groups are split up into single
-- definitions.
traverseContext1 ::
  (Monoid t, Applicative f, Data a, Data b, Data c) =>
  -- | process one definition
  (NameSymbol.T -> Context.Definition a b c -> f t) ->
  Context.T a b c ->
  f t
traverseContext1 = traverseContext . foldMapA . onEntry

-- | Same as 'traverseContext1', but ignoring the return value.
traverseContext1_ ::
  (Applicative f, Data a, Data b, Data c) =>
  -- | process one definition
  (NameSymbol.T -> Context.Definition a b c -> f z) ->
  Context.T a b c ->
  f ()
traverseContext1_ = traverseContext_ . traverse_ . onEntry

onEntry ::
  (NameSymbol.T -> Context.Definition term ty sumRep -> t) ->
  Entry term ty sumRep ->
  t
onEntry f (Entry {name, def}) = f name def

-- | Sorts a context by dependency order. Each element of the output is
-- a mutually-recursive group, whose elements depend only on each other and
-- elements of previous groups. The first element of each pair is its
-- fully-qualified name.
recGroups ::
  (Data term, Data ty, Data sumRep) =>
  Context.T term ty sumRep ->
  [Group term ty sumRep]
recGroups (Context.T curns _ top) =
  let (groups, deps) = run_ curns $ recGroups' $ toNameSpace top
      get n = maybe [] toList $ HashMap.lookup n deps
      edges = map (\(n, gs) -> (gs, n, get n)) $ HashMap.toList groups
      (g, fromV', _) = Graph.graphFromEdges edges
      fromV v = let (gs, _, _) = fromV' v in gs
   in Graph.topSort g |> reverse |> concatMap fromV

recGroups' ::
  (Data term, Data ty, Data sumRep) =>
  Context.NameSpace term ty sumRep ->
  Env term ty sumRep ()
recGroups' ns = do
  defs <- fmap concat $ for (NameSpace.toList1' ns) \(name, def) ->
    case def of
      Context.Record ns _ -> do
        withPrefix name $ recGroups' ns
        pure []
      Context.CurrentNameSpace -> withPrefix name do
        ask @"curNameSpace" >>= recGroups'
        pure []
      _ -> do
        qname <- qualify name
        pure [(def, qname, fv def)]
  let (g, fromV, _) = Graph.graphFromEdges defs
  let accum1 xs v =
        let (def, name, ys) = fromV v
         in (xs <> HashSet.fromList ys, Entry {name, def})
  let accum xs vs = let (ys, es) = mapAccumL accum1 [] vs in (xs <> ys, es)
  let (fvs, groups) =
        Graph.scc g
          |> mapAccumL (\xs t -> accum xs (toList t)) HashSet.empty
  addDeps fvs
  for_ groups addGroup

fv :: Data a => a -> [NameSymbol.T]
fv = HashSet.toList . SYB.everything (<>) (SYB.mkQ mempty (FV.op []))

toNameSpace :: HashMap.T Symbol a -> NameSpace.T a
toNameSpace public = NameSpace.T {public, private = mempty}
