module Juvix.FrontendContextualise.Environment where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Juvix.Core.Common.Context as Context
import Juvix.Library

data Environment term0 ty0 sumRep0 termN tyN sumRepN
  = Env
      { old :: Context.T term0 ty0 sumRep0,
        new :: Context.T termN tyN sumRepN
      }
  deriving (Generic)

type ContextAlias term0 ty0 sumRep0 termN tyN sumRepN = State (Environment term0 ty0 sumRep0 termN tyN sumRepN)

newtype Context term0 ty0 sumRep0 termN tyN sumRepN a = Ctx {antiAlias :: ContextAlias term0 ty0 sumRep0 termN tyN sumRepN a}
  deriving (Functor, Applicative, Monad)
  deriving
    ( HasReader "old" (Context.T term0 ty0 sumRep0),
      HasSource "old" (Context.T term0 ty0 sumRep0)
    )
    via ReaderField "old" (ContextAlias term0 ty0 sumRep0 termN tyN sumRepN)
  deriving
    ( HasState "new" (Context.T termN tyN sumRepN),
      HasSink "new" (Context.T termN tyN sumRepN),
      HasSource "new" (Context.T termN tyN sumRepN)
    )
    via StateField "new" (ContextAlias term0 ty0 sumRep0 termN tyN sumRepN)

modify ::
  HasState "new" (Context.T term ty sumRep) m =>
  ( Context.Definition term ty sumRep ->
    Maybe (Context.Definition term ty sumRep)
  ) ->
  Symbol ->
  m ()
modify f sy = Juvix.Library.modify @"new" (Context.modify f sy)

-- lookup :: Symbol -> Context term0 ty0 sumRep0 termN tyN sumRepN (Maybe (Context.Definition termN tyN sumRepN))
lookup ::
  HasState "new" (Context.T term ty sumRep) m =>
  Symbol ->
  m (Maybe (Context.Definition term ty sumRep))
lookup sy = do
  ctx <- get @"new"
  return $ Context.lookup sy ctx

-- ask :: Symbol -> Context term0 ty0 sumRep0 termN tyN sumRepN (Maybe (Context.Definition term0 ty0 sumRep0))
ask ::
  HasReader "old" (Context.T term ty sumRep) m =>
  Symbol ->
  m (Maybe (Context.Definition term ty sumRep))
ask sy = do
  ctx <- Juvix.Library.ask @"old"
  return $ Context.lookup sy ctx

-- mapWithKey :: (Symbol -> Context.Definition termN tyN sumRepN -> Context.Definition termN tyN sumRepN) -> Context term0 ty0 sumRep0 termN tyN sumRepN ()
mapWithKey ::
  HasState "new" (Context.T term ty sumRep) m =>
  ( Symbol ->
    Context.Definition term ty sumRep ->
    Context.Definition term ty sumRep
  ) ->
  m ()
mapWithKey f = Juvix.Library.modify @"new" (Context.mapWithKey f)

-- add :: Symbol -> Context.Definition termN tyN sumRepN -> Context term0 ty0 sumRep0 termN tyN sumRepN ()
add ::
  HasState "new" (Context.T term ty sumRep) m =>
  Symbol ->
  Context.Definition term ty sumRep ->
  m ()
add sy def = Juvix.Library.modify @"new" (Context.add sy def)

-- remove :: Symbol -> Context term0 ty0 sumRep0 termN tyN sumRepN ()
remove ::
  HasState "new" (Context.T term ty sumRep) m =>
  Symbol ->
  m ()
remove sy = Juvix.Library.modify @"new" (Context.remove sy)

-- forKey :: Context term0 ty0 sumRep0 termN tyN sumRepN () -> [Symbol -> Context.Definition term0 ty0 sumRep0 -> Context term0 ty0 sumRep0 termM tyM sumRepM a] -> Context term0 ty0 sumRep termM tyM sumRepM a
forKey = undefined

--TODO transLike :: NonEmpty functionLike -> Maybe Signature -> Maybe Usage -> Definition
transLike = undefined

-- addUnknown ::
--   HasState "new" (Context.T term ty sumRep) m => NonEmpty Symbol -> m ()
addUnknown ::
  HasState "new" (Context.T term ty sumRep) m =>
  NonEmpty Symbol ->
  m ()
addUnknown sym =
  Juvix.Library.modify @"new"
    (Context.add (NonEmpty.head sym) (Context.Unknown Nothing))
