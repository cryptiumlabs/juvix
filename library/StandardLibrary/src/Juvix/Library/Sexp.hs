{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Juvix.Library.Sexp where

import Control.Lens hiding ((|>))
import Juvix.Library hiding (foldr, show)
import qualified Juvix.Library as Std
import qualified Juvix.Library.LineNum as LineNum
import qualified Juvix.Library.NameSymbol as NameSymbol
import Prelude (error)
import Prelude (Show (..), String)

-- TODO ∷ make Atom generic, and have it conform to an interface?
-- This way we can erase information later!
data T
  = Atom Atom
  | Cons {tCar :: T, tCdr :: T}
  | Nil
  deriving (Eq)

data Atom
  = A {atomName :: NameSymbol.T, atomLineNum :: Maybe LineNum.T}
  deriving (Show)

instance Eq Atom where
  A n1 _ == A n2 _ = n1 == n2

makeLensesWith camelCaseFields ''Atom

foldPred :: T -> (NameSymbol.T -> Bool) -> (Atom -> T -> T) -> T
foldPred t pred f =
  case t of
    Cons (Atom atom) xs
      | pred (atom ^. name) ->
        foldPred (f atom xs) pred f
    Cons cs xs ->
      Cons (foldPred cs pred f) (foldPred xs pred f)
    Nil -> Nil
    Atom a -> Atom a

foldr :: (T -> p -> p) -> p -> T -> p
foldr f acc ts =
  case ts of
    Cons a as -> f a (foldr f acc as)
    Atom ____ -> f ts acc
    Nil -> acc

foldr1 :: (T -> T -> T) -> T -> Maybe T
foldr1 f (Cons x xs) = Just $ unsafe (Cons x xs)
  where
    unsafe ts =
      case ts of
        Cons a Nil -> a
        Cons a cds -> f a (unsafe cds)
        _ -> error "doesn't happen"
foldr1 _ _empty = Nothing

butLast :: T -> T
butLast (Cons _ Nil) = Nil
butLast (Cons x xs) = Cons x (butLast xs)
butLast (Atom a) = Atom a
butLast Nil = Nil

last :: T -> T
last (Cons x Nil) = x
last (Cons _ xs) = last xs
last (Atom a) = Atom a
last Nil = Nil

list :: Foldable t => t T -> T
list = Std.foldr Cons Nil

listStar :: [T] -> T
listStar = fromMaybe Nil . foldr1May Cons

addMetaToCar :: Atom -> T -> T
addMetaToCar (A _ lineInfo) (Cons (Atom (A term _)) xs) =
  Cons (Atom (A term lineInfo)) xs
addMetaToCar _ xs = xs

car :: T -> T
car (Cons x _) = x
car Nil = Nil
car (Atom a) = Atom a

cdr :: T -> T
cdr (Cons _ xs) = xs
cdr Nil = Nil
cdr (Atom a) = Atom a

atom :: NameSymbol.T -> T
atom x = Atom $ A x Nothing

showNoParens :: T -> String
showNoParens (Cons car cdr)
  | showNoParens cdr == ")" =
    show car <> showNoParens cdr
  | otherwise =
    show car <> " " <> showNoParens cdr
showNoParens Nil = ")"
showNoParens xs = show xs

isAtomNamed :: T -> NameSymbol.T -> Bool
isAtomNamed (Atom (A name _)) name2 = name == name2
isAtomNamed _ _ = False

nameFromT :: T -> Maybe NameSymbol.T
nameFromT (Atom (A name _)) = Just name
nameFromT _ = Nothing

-- TODO ∷ make reader instance

instance Show T where
  show (Cons car cdr)
    | show cdr == ")" =
      "(" <> show car <> showNoParens cdr
    | otherwise =
      "(" <> show car <> " " <> showNoParens cdr
  show (Atom (A x _)) =
    show (NameSymbol.toSymbol x)
  show Nil = "nil"
