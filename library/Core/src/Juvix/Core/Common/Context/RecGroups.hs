{-# LANGUAGE OverloadedLists #-}
-- | Calculate mutually-recursive groups of definitions.
module Juvix.Core.Common.Context.RecGroups
  ( Entry (..),
    Group,
    Groups,
    recGroups,
  )
where

import Juvix.Core.Common.Context.RecGroups.Types
import qualified Juvix.Core.Common.Context.Types as Context
import qualified Juvix.Core.Common.NameSpace as NameSpace
import qualified Juvix.Library.HashMap as HashMap
import qualified Juvix.Library.NameSymbol as NameSymbol
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
-- import qualified Juvix.FrontendContextualise.InfixPrecedence.Types as FE
import Juvix.Library
-- import Generics.SYB
import qualified Juvix.FrontendContextualise.InfixPrecedence.FreeVars as FV
import qualified Data.Graph as Graph
import qualified Generics.SYB as SYB

-- | Sorts a context by dependency order. Each element of the output is
-- a mutually-recursive group, whose elements depend only on each other and
-- elements of previous groups. The first element of each pair is its
-- fully-qualified name.
recGroups :: (Data term, Data ty, Data sumRep)
          => Context.T term ty sumRep -> [Group term ty sumRep]
recGroups (Context.T curns _ top) =
  let (groups, deps) = run_ curns $ recGroups' $ toNameSpace top in
  let get n = maybe [] toList $ HashMap.lookup n deps in
  let edges = map (\(n, gs) -> (gs, n, get n)) $ HashMap.toList groups in
  let (g, fromV', _) = Graph.graphFromEdges edges in
  let fromV v = let (gs, _, _) = fromV' v in gs in
  Graph.topSort g |> reverse |> concatMap fromV

recGroups' :: (Data term, Data ty, Data sumRep)
           => Context.NameSpace term ty sumRep -> Env term ty sumRep ()
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
        let (def, name, ys) = fromV v in
        (xs <> HashSet.fromList ys, Entry {name, def})
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
