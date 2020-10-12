-- |
-- - This implements the Shunt Yard algorithm for determining the
--   precedence of operations
module Juvix.FrontendContextualise.InfixPrecedence.ShuntYard where

import Juvix.Library hiding (Associativity, div)
import Prelude (error)

data Associativity
  = Left'
  | Right'
  | NonAssoc
  deriving (Eq, Show)

data Precedence sym = Pred sym Associativity Int
  deriving (Show, Eq)

data Error sym
  = Clash (Precedence sym) (Precedence sym)
  | MoreEles
  deriving (Show, Eq)

-- Not a real ordering, hence not an ord instance
predOrd :: Precedence sym -> Precedence sym -> Either (Error sym) Bool
predOrd p1@(Pred _ fix iNew) p2@(Pred _ fix' iOld)
  | iNew == iOld && fix /= fix' =
    Left (Clash p1 p2)
  | iNew == iOld && NonAssoc == fix && NonAssoc == fix' =
    Left (Clash p1 p2)
  | otherwise =
    case fix of
      Left' ->
        Right (iNew <= iOld)
      _ ->
        Right (iNew < iOld)

data PredOrEle sym a
  = Precedence (Precedence sym)
  | Ele a
  deriving (Eq, Show)

data Application sym a
  = App sym (Application sym a) (Application sym a)
  | Single a
  deriving (Eq, Show)

shunt :: NonEmpty (PredOrEle sym a) -> Either (Error sym) (Application sym a)
shunt = fmap (combine . popAll) . foldM shuntAcc ([], [])

shuntAcc ::
  ([Application sym a], [Precedence sym]) ->
  PredOrEle sym a ->
  Either (Error sym) ([Application sym a], [Precedence sym])
shuntAcc (aps, prec) (Ele a) =
  Right (Single a : aps, prec)
shuntAcc (aps, []) (Precedence p) =
  Right (aps, [p])
shuntAcc (aps, (pred : preds)) (Precedence p) =
  case p `predOrd` pred of
    Right True ->
      case aps of
        x1 : x2 : xs ->
          Right (App (predSymbol pred) x2 x1 : xs, p : preds)
        [_] ->
          Left MoreEles
        [] ->
          Left MoreEles
    Right False ->
      Right (aps, p : pred : preds)
    Left err ->
      Left err

popAll :: ([Application sym a], [Precedence sym]) -> [Application sym a]
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
combine :: [Application sym a] -> Application sym a
combine (x : _) = x
combine [] =
  error "precondition failed: Shunt.combine was given an empty list"

predSymbol :: Precedence sym -> sym
predSymbol (Pred s _ _) = s
