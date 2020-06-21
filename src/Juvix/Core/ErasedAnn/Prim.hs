-- |
-- - This module represents the type which will be sent to the
--   paramiterization
-- - the =Take= type is what a paramiterization will take coming in
-- - the =Return= type is what will be handed back to Core to evaluate
--   and decide on the next steps. If this is a =Left= type checking
--   has failed, if it's a =Right= then type checking will continue
module Juvix.Core.ErasedAnn.Prim where

import qualified Juvix.Core.ErasedAnn.Types as Types
import qualified Juvix.Core.Usage as Usage
import Juvix.Library hiding (Type)

data Return primTy primVal
  = -- arguments left
    Cont
      { fun :: Take primTy primVal,
        args :: [Take primTy primVal],
        numLeft :: Natural
      }
  | Return primVal
  deriving (Show, Eq, Generic)

data Take primTy primVal = Take
  { usage :: Usage.T,
    type' :: Types.Type primTy primVal,
    term :: primVal
  }
  deriving (Show, Eq, Generic)

fromAnn :: Types.AnnTerm primTy primVal -> Maybe (Take primTy primVal)
fromAnn (Types.Ann usage type' p) =
  case p of
    Types.Prim p -> Just (Take usage type' p)
    _ -> Nothing

toAnn :: Take primTy primVal -> Types.AnnTerm primTy primVal
toAnn (Take usage type' term) = Types.Ann usage type' (Types.Prim term)
