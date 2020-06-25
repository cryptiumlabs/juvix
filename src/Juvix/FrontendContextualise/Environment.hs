module Juvix.FrontendContextualise.Environment where

import Juvix.Library
import qualified Juvix.Core.Common.Context as Context

data Environment term0 ty0 sumRep0 termN tyN sumRepN =
  Env { old :: Context.T term0 ty0 sumRep0
      , new :: Context.T termN tyN sumRepN
      }
  deriving (Generic)

type ContextAlias term0 ty0 sumRep0 termN tyN sumRepN = State (Environment term0 ty0 sumRep0 termN tyN sumRepN)

newtype Context term0 ty0 sumRep0 termN tyN sumRepN a = Ctx { antiAlias :: ContextAlias term0 ty0 sumRep0 termN tyN sumRepN a }
  deriving (Functor, Applicative, Monad)
  deriving (HasReader "old" (Context.T term0 ty0 sumRep0),
            HasSource "old" (Context.T term0 ty0 sumRep0))
    via ReaderField "old" (ContextAlias term0 ty0 sumRep0 termN tyN sumRepN)
  deriving (HasState "new" (Context.T termN tyN sumRepN),
            HasSink "new" (Context.T termN tyN sumRepN),
            HasSource "new" (Context.T termN tyN sumRepN))
    via StateField "new" (ContextAlias term0 ty0 sumRep0 termN tyN sumRepN)

modify :: (Context.Definition termN tyN sumRepN -> Maybe (Context.Definition termN tyN sumRepN)) ->
    Symbol -> Context term0 ty0 sumRep0 termN tyN sumRepN ()
modify f sy = Juvix.Library.modify @"new" (Context.modify f sy)

lookup :: Symbol -> Context term0 ty0 sumRep0 termN tyN sumRepN (Maybe (Context.Definition termN tyN sumRepN))
lookup sy = do
  ctx <- get @"new"
  return $ Context.lookup sy ctx

ask :: Symbol -> Context term0 ty0 sumRep0 termN tyN sumRepN (Maybe (Context.Definition term0 ty0 sumRep0))
ask sy = do
  ctx <- Juvix.Library.ask @"old"
  return $ Context.lookup sy ctx

mapWithKey :: (Symbol -> Context.Definition termN tyN sumRepN -> Context.Definition termN tyN sumRepN) -> Context term0 ty0 sumRep0 termN tyN sumRepN ()
mapWithKey f = Juvix.Library.modify @"new" (Context.mapWithKey f)

add :: Symbol -> Context.Definition termN tyN sumRepN -> Context term0 ty0 sumRep0 termN tyN sumRepN ()
add sy def = Juvix.Library.modify @"new" (Context.add sy def)

forKey :: Context term0 ty0 sumRep0 termN tyN sumRepN () -> [Symbol -> Context.Definition term0 ty0 sumRep0 -> Context term0 ty0 sumRep0 termM tyM sumRepM a] -> Context term0 ty0 sumRep termM tyM sumRepM a
forKey = undefined
