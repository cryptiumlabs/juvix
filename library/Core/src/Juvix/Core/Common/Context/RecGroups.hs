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
import Juvix.Library

-- | Sorts a context by dependency order. Each element of the output is
-- a mutually-recursive group, whose elements depend only on each other and
-- elements of previous groups. The first element of each pair is its
-- fully-qualified name.
recGroups :: Context.T term ty sumRep -> Groups term ty sumRep
recGroups = run_ . recGroups' . toNameSpace . Context.topLevelMap

-- TODO: do actual calculation
-- (returns every definition in its own group for now)
recGroups' :: Context.NameSpace term ty sumRep -> Env term ty sumRep ()
recGroups' ns = do
  for_ (NameSpace.toList1' ns) \(name, def) -> do
    newGroup
    addDef name def
    case def of
      Context.Record ns _ -> withPrefix name $ recGroups' ns
      _ -> pure ()

toNameSpace :: HashMap.T Symbol a -> NameSpace.T a
toNameSpace public = NameSpace.T {public, private = mempty}
