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
          => Context.T term ty sumRep -> Groups term ty sumRep
recGroups (Context.T curns _ top) = run_ curns $ recGroups' $ toNameSpace top

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
        pure [((name, def), qname, fv def)]
  let (g, fromV, _) = Graph.graphFromEdges defs
  let unV v = let (nameDef, _, _) = fromV v in nameDef
  let groups = Graph.scc g |> reverse |> map (toList . map unV)
  for_ groups \ds -> do
    newGroup
    for_ ds \(name, def) -> addDef name def
  -- TODO put namespaces in order


fv :: Data a => a -> [NameSymbol.T]
fv = HashSet.toList . SYB.everything (<>) (SYB.mkQ mempty (FV.op []))


toNameSpace :: HashMap.T Symbol a -> NameSpace.T a
toNameSpace public = NameSpace.T {public, private = mempty}
