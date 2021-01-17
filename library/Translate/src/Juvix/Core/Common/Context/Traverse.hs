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

import qualified Data.DList as D
import qualified Data.Graph as Graph
import qualified Data.HashSet as HashSet
import qualified Generics.SYB as SYB
import qualified Juvix.Core.Common.Context as Context
import Juvix.Core.Common.Context.Traverse.Types
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
recGroups ctx@(Context.T _ _ top) =
  let (groups, deps) = run_ ctx $ recGroups' injectTopLevel $ toNameSpace top
      get n = maybe [] toList $ HashMap.lookup n deps
      edges = map (\(n, gs) -> (gs, n, get n)) $ HashMap.toList groups
      (g, fromV', _) = Graph.graphFromEdges edges
      fromV v = let (gs, _, _) = fromV' v in gs
   in Graph.topSort g |> reverse |> concatMap fromV

injectTopLevel :: (Semigroup a, IsString a) => a -> a
injectTopLevel name = Context.topLevelName <> "." <> name

recGroups' ::
  HasRecGroups term ty sumRep m =>
  (Symbol -> Symbol) ->
  Context.NameSpace term ty sumRep ->
  m ()
recGroups' injection ns = do
  defs <- concat <$> for (NameSpace.toList1' ns) \(name, def) ->
    case def of
      Context.Record ns _ -> do
        contextName <- gets @"context" Context.currentName
        modify @"context"
          ( \ctx ->
              fromMaybe
                ctx
                ( Context.qualifyLookup
                    (NameSymbol.fromSymbol (injection name))
                    ctx
                    >>= (`Context.inNameSpace` ctx)
                )
          )
        withPrefix name $ recGroups' identity ns
        modify @"context"
          (\ctx -> fromMaybe ctx (Context.inNameSpace contextName ctx))
        pure []
      Context.CurrentNameSpace -> withPrefix name do
        curNS <- gets @"context" Context.currentNameSpace
        recGroups' identity curNS
        pure []
      _ -> do
        qname <- qualify name
        fvs <- fv def
        pure [(def, qname, fvs)]
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

fv :: (ContextReader term ty sumRep m, Data a) => a -> m [NameSymbol.T]
fv t = gets @"context" \ctx ->
  SYB.everything (<>) (SYB.mkQ mempty (FV.op [])) t
    |> HashSet.toList
    |> mapMaybe (`Context.qualifyLookup` ctx)

toNameSpace :: HashMap.T Symbol a -> NameSpace.T a
toNameSpace public = NameSpace.T {public, private = mempty}

-- | Add a group to the final output.
addGroup ::
  (PrefixReader m, OutputState term ty sumRep m, Foldable t) =>
  t (Entry term ty sumRep) ->
  m ()
addGroup grp = do
  prefix <- prefixM
  case nonEmpty $ toList grp of
    Just grp -> modify @"output" $ HashMap.alter f prefix
      where
        f = Just . maybe [grp] (<> [grp])
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
qualify n = asks @"prefix" (`applyPrefix` n)

-- | Apply a prefix to a name.
applyPrefix :: Prefix -> Symbol -> NameSymbol.T
applyPrefix (P pfx) = NameSymbol.qualify1 pfx
