-- | Calculate mutually-recursive groups of definitions.
module Juvix.Core.Common.Context.RecGroups
  ( Entry (..),
    Group,
    Groups,
    recGroups,
  )
where

import Juvix.Core.Common.Context.RecGroups.Types
import Juvix.Core.Common.Context.Types
import qualified Juvix.Core.Common.NameSpace as NS
import Juvix.Library

-- | Sorts a context by dependency order. Each element of the output is
-- a mutually-recursive group, whose elements depend only on each other and
-- elements of previous groups. The first element of each pair is its
-- fully-qualified name.
recGroups :: T term ty sumRep -> Groups term ty sumRep
recGroups = run_ . recGroups' . currentNameSpace

-- TODO: do actual calculation
-- (returns every definition in its own group for now)
recGroups' :: NameSpace term ty sumRep -> Env term ty sumRep ()
recGroups' ns = do
  for_ (NS.toList1' ns) \(name, def) -> do
    newGroup
    addDef name def
    case def of
      Record ns _ -> withPrefix name $ recGroups' ns
      _ -> pure ()
