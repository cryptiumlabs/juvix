module Juvix.FrontendContextualise.Environment where

import qualified Juvix.Core.Common.Context as Context
import qualified Juvix.Core.Common.NameSpace as NameSpace
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol

type HasNew t ty s m = HasState "new" (Context.T t ty s) m

type HasOld t ty s m = HasState "old" (Context.T t ty s) m

-- to avoid casting all the time

class SymbLookup a where
  look ::
    a ->
    Context.T term ty sumRep ->
    Maybe (Context.From (Context.Definition term ty sumRep))

  lookCurr ::
    a ->
    Context.T term ty sumRep ->
    Maybe (NameSpace.From (Context.Definition term ty sumRep))

instance SymbLookup Symbol where
  look sym cont = Context.lookup (NameSymbol.fromSymbol sym) cont

  --
  lookCurr sym cont = Context.lookupCurrent (NameSymbol.fromSymbol sym) cont

instance SymbLookup NameSymbol.T where
  look sym cont = Context.lookup sym cont

  lookCurr sym cont = Context.lookupCurrent sym cont

lookup ::
  (HasNew term ty sumRep m, SymbLookup sym) =>
  sym ->
  m (Maybe (Context.From (Context.Definition term ty sumRep)))
lookup sy = do
  ctx <- get @"new"
  return $ look sy ctx

lookupCurrent ::
  (HasNew term ty sumRep m, SymbLookup sym) =>
  sym ->
  m (Maybe (NameSpace.From (Context.Definition term ty sumRep)))
lookupCurrent sy = do
  ctx <- get @"new"
  return $ lookCurr sy ctx

ask ::
  (HasOld term ty sumRep m, SymbLookup sym) =>
  sym ->
  m (Maybe (Context.From (Context.Definition term ty sumRep)))
ask sy = do
  ctx <- get @"old"
  return $ look sy ctx

add ::
  HasNew term ty sumRep m =>
  NameSpace.From Symbol ->
  Context.Definition term ty sumRep ->
  m ()
add sy def = Juvix.Library.modify @"new" (Context.add sy def)

addGlobal ::
  HasNew term ty sumRep m =>
  NameSymbol.T ->
  Context.Definition term ty sumRep ->
  m ()
addGlobal sy def = Juvix.Library.modify @"new" (Context.addGlobal sy def)

remove ::
  HasNew term ty sumRep m => NameSpace.From Symbol -> m ()
remove sy = Juvix.Library.modify @"new" (Context.remove sy)

removeGlobal ::
  HasNew term ty sumRep m => NameSymbol.T -> m ()
removeGlobal sy = Juvix.Library.modify @"new" (Context.removeNameSpace sy)

removeOld ::
  HasOld term ty sumRep m => NameSpace.From Symbol -> m ()
removeOld sy = Juvix.Library.modify @"old" (Context.remove sy)

addUnknown ::
  HasNew term ty sumRep m => NameSpace.From Symbol -> m ()
addUnknown sym =
  Juvix.Library.modify @"new"
    (Context.add sym (Context.Unknown Nothing))

addUnknownGlobal ::
  HasNew term ty sumRep m => Context.From Symbol -> m ()
addUnknownGlobal (Context.Current sym) = addUnknown sym
addUnknownGlobal (Context.Outside sym) =
  Juvix.Library.modify @"new"
    (Context.addGlobal (pure sym) (Context.Unknown Nothing))
