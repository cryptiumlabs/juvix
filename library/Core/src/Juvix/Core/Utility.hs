module Juvix.Core.Utility where

import Data.List ((!!), findIndex, tail)
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol

pushName ::
  HasState "symbolStack" [sym] m => sym -> m ()
pushName name = modify @"symbolStack" (name :)

popName :: HasState "symbolStack" [sym] m => m ()
popName = modify @"symbolStack" tail

-- FIXME some error message if the stack is empty?

withName ::
  HasState "symbolStack" [sym] m => sym -> m a -> m a
withName n act = pushName n *> act <* popName

lookupName ::
  (Eq sym, HasState "symbolStack" [sym] m) => sym -> m (Maybe Int)
lookupName name = do
  stack <- get @"symbolStack"
  let ind = findIndex (== name) stack
  pure ind

unDeBruijn ::
  HasState "nameStack" [Int] m => Int -> m NameSymbol.T
unDeBruijn ind = do
  stack <- get @"nameStack"
  pure (NameSymbol.fromString $ show $ stack !! ind)

newName ::
  ( HasState "nextName" Int m,
    HasState "nameStack" [Int] m
  ) =>
  m NameSymbol.T
newName = do
  name <- nextName
  modify @"nameStack" (name :)
  return (NameSymbol.fromString $ show name)

nextName :: (HasState "nextName" s f, Enum s) => f s
nextName = get @"nextName" <* modify @"nextName" succ
