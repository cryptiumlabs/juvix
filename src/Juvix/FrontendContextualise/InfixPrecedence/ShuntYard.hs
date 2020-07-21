-- |
-- - This implements the Shunt Yard algorithm for determining the
--   precedence of operations
module Juvix.FrontendContextualise.InfixPrecedence.ShuntYard where

import Juvix.Library hiding (Associativity, Left, Right, div)
import Prelude (error)

data Associativity
  = Left
  | Right
  | NonAssoc
  deriving (Eq, Show)

data Precedence = Pred Symbol Associativity Int
  deriving (Show, Eq)

-- Not a real ordering, hence not an ord instance
predOrd :: Precedence -> Precedence -> Bool
predOrd (Pred _ Left i) (Pred _ _ iOld) = i <= iOld
predOrd (Pred _ _ iNew) (Pred _ _ iOld) = iNew < iOld

data PredOrEle a
  = Precedence Precedence
  | Ele a
  deriving (Eq, Show)

data Application a
  = App Symbol (Application a) (Application a)
  | Single a
  deriving (Eq, Show)

shunt :: NonEmpty (PredOrEle a) -> Application a
shunt = combine . popAll . foldl' shuntAcc ([], [])

shuntAcc ::
  ([Application a], [Precedence]) -> PredOrEle a -> ([Application a], [Precedence])
shuntAcc (aps, prec) (Ele a) =
  (Single a : aps, prec)
shuntAcc (aps, []) (Precedence p) =
  (aps, [p])
shuntAcc (aps, (pred : preds)) (Precedence p)
  | p `predOrd` pred =
    case aps of
      x1 : x2 : xs ->
        (App (predSymbol pred) x2 x1 : xs, p : preds)
      [_] ->
        error "More applications than elements!"
      [] ->
        error "More applications than elements!"
  | otherwise =
    (aps, p : pred : preds)

popAll :: ([Application a], [Precedence]) -> [Application a]
popAll (xs, []) =
  xs
popAll (x1 : x2 : xs, op : ops) =
  popAll (App (predSymbol op) x2 x1 : xs, ops)
popAll ([], (_ : _)) =
  error "More applications than elements!"
popAll ([_], (_ : _)) =
  error "More applications than elements!"

-- This list should be of length 1 after all is said and done, and an
-- application given by shunt
combine :: [Application a] -> Application a
combine (x : _) = x
combine [] =
  error "precondition failed: Shunt.combine was given an empty list"

predSymbol :: Precedence -> Symbol
predSymbol (Pred s _ _) = s
