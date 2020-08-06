module Juvix.FrontendContextualise.Environment where

import qualified Juvix.Core.Common.Context as Context
import qualified Juvix.Core.Common.NameSpace as NameSpace
import Juvix.Library

type HasNew t ty s m = HasState "new" (Context.T t ty s) m

type HasOld t ty s m = HasState "old" (Context.T t ty s) m

lookup ::
  HasNew term ty sumRep m =>
  Symbol ->
  m (Maybe (Context.From (Context.Definition term ty sumRep)))
lookup sy = do
  ctx <- get @"new"
  return $ Context.lookup sy ctx

ask ::
  HasOld term ty sumRep m =>
  Symbol ->
  m (Maybe (Context.From (Context.Definition term ty sumRep)))
ask sy = do
  ctx <- get @"old"
  return $ Context.lookup sy ctx

add ::
  HasNew term ty sumRep m =>
  NameSpace.From Symbol ->
  Context.Definition term ty sumRep ->
  m ()
add sy def = Juvix.Library.modify @"new" (Context.add sy def)

remove ::
  HasNew term ty sumRep m => NameSpace.From Symbol -> m ()
remove sy = Juvix.Library.modify @"new" (Context.remove sy)

removeOld ::
  HasOld term ty sumRep m => NameSpace.From Symbol -> m ()
removeOld sy = Juvix.Library.modify @"old" (Context.remove sy)

addUnknown ::
  HasNew term ty sumRep m => NameSpace.From Symbol -> m ()
addUnknown sym =
  Juvix.Library.modify @"new"
    (Context.add sym (Context.Unknown Nothing))
