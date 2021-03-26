module Juvix.Core.Utility where

import Data.List ((!!), findIndex)
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol

withSym :: HasReader "symbolStack" [sym] m => sym -> m a -> m a
withSym name = local @"symbolStack" (name :)

lookupName :: (Eq sym, HasReader "symbolStack" [sym] m) => sym -> m (Maybe Int)
lookupName name = findIndex (== name) <$> ask @"symbolStack"

unDeBruijn :: HasReader "nameStack" [Int] m => Int -> m NameSymbol.T
unDeBruijn ind = do
  stack <- ask @"nameStack"
  pure $ NameSymbol.fromString $ show $ stack !! ind

withName ::
  (HasState "nextName" Int m, HasReader "nameStack" [Int] m) =>
  (NameSymbol.T -> m a) -> m a
withName act = do
  name <- nextName
  let sym = NameSymbol.fromString $ show name
  local @"nameStack" (name :) $ act sym

nextName :: (HasState "nextName" s f, Enum s) => f s
nextName = get @"nextName" <* modify @"nextName" succ
