module Juvix.FrontendContextualise.Environment where

import qualified Juvix.Core.Common.Context as Context
import Juvix.Library

data Environment term0 ty0 sumRep0 termN tyN sumRepN
  = Env
      { old :: Context.T term0 ty0 sumRep0,
        new :: Context.T termN tyN sumRepN
      }
  deriving (Generic)

type ContextAlias term0 ty0 sumRep0 termN tyN sumRepN =
  State (Environment term0 ty0 sumRep0 termN tyN sumRepN)

newtype Context term0 ty0 sumRep0 termN tyN sumRepN a
  = Ctx {antiAlias :: ContextAlias term0 ty0 sumRep0 termN tyN sumRepN a}
  deriving (Functor, Applicative, Monad)
  deriving
    ( HasState "old" (Context.T term0 ty0 sumRep0),
      HasSink "old" (Context.T term0 ty0 sumRep0),
      HasSource "old" (Context.T term0 ty0 sumRep0)
    )
    via StateField "old" (ContextAlias term0 ty0 sumRep0 termN tyN sumRepN)
  deriving
    ( HasState "new" (Context.T termN tyN sumRepN),
      HasSink "new" (Context.T termN tyN sumRepN),
      HasSource "new" (Context.T termN tyN sumRepN)
    )
    via StateField "new" (ContextAlias term0 ty0 sumRep0 termN tyN sumRepN)

type HasNew t ty s m = HasState "new" (Context.T t ty s) m

modify ::
  HasNew term ty sumRep m =>
  ( Context.Definition term ty sumRep ->
    Maybe (Context.Definition term ty sumRep)
  ) ->
  Symbol ->
  m ()
modify f sy = Juvix.Library.modify @"new" (Context.modify f sy)

lookup ::
  HasNew term ty sumRep m =>
  Symbol ->
  m (Maybe (Context.Definition term ty sumRep))
lookup sy = do
  ctx <- get @"new"
  return $ Context.lookup sy ctx

ask ::
  HasReader "old" (Context.T term ty sumRep) m =>
  Symbol ->
  m (Maybe (Context.Definition term ty sumRep))
ask sy = do
  ctx <- Juvix.Library.ask @"old"
  return $ Context.lookup sy ctx

mapWithKey ::
  HasNew term ty sumRep m =>
  ( Symbol ->
    Context.Definition term ty sumRep ->
    Context.Definition term ty sumRep
  ) ->
  m ()
mapWithKey f = Juvix.Library.modify @"new" (Context.mapWithKey f)

add ::
  HasNew term ty sumRep m => Symbol -> Context.Definition term ty sumRep -> m ()
add sy def = Juvix.Library.modify @"new" (Context.add sy def)

remove ::
  HasNew term ty sumRep m => Symbol -> m ()
remove sy = Juvix.Library.modify @"new" (Context.remove sy)

removeOld :: HasState "old" (Context.T term ty sumRep) m => Symbol -> m ()
removeOld sy = Juvix.Library.modify @"old" (Context.remove sy)

--TODO transLike :: NonEmpty functionLike -> Maybe Signature -> Maybe Usage -> Definition
transLike = undefined

addUnknown ::
  HasState "new" (Context.T term ty sumRep) m => Symbol -> m ()
addUnknown sym =
  Juvix.Library.modify @"new"
    (Context.add sym (Context.Unknown Nothing))
