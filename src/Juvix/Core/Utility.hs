module Juvix.Core.Utility where

import Data.List (findIndex, tail, (!!))
import Juvix.Library

pushName ::
  HasState "symbolStack" [Symbol] m => Symbol -> m ()
pushName name = modify @"symbolStack" (name :)

popName :: HasState "symbolStack" [Symbol] m => m ()
popName = modify @"symbolStack" tail
  -- FIXME some error message if the stack is empty?

withName ::
  HasState "symbolStack" [Symbol] m => Symbol -> m a -> m a
withName n act = pushName n *> act <* popName

lookupName ::
  HasState "symbolStack" [Symbol] m => Symbol -> m (Maybe Int)
lookupName name = do
  stack <- get @"symbolStack"
  let ind = findIndex (== name) stack
  pure ind

unDeBruijin ::
  HasState "nameStack" [Int] m => Int -> m Symbol
unDeBruijin ind = do
  stack <- get @"nameStack"
  pure (intern $ show $ stack !! ind)

newName ::
  ( HasState "nextName" Int m,
    HasState "nameStack" [Int] m
  ) =>
  m Symbol
newName = do
  name <- nextName
  modify @"nameStack" (name :)
  return (intern (show name))

nextName :: (HasState "nextName" s f, Enum s) => f s
nextName = get @"nextName" <* modify @"nextName" succ
