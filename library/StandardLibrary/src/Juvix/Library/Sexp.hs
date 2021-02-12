{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Juvix.Library.Sexp where

import Control.Lens hiding ((|>))
import Juvix.Library hiding (foldr, show)
import qualified Juvix.Library as Std
import qualified Juvix.Library.LineNum as LineNum
import qualified Juvix.Library.NameSymbol as NameSymbol
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
  deriving (Show, Eq)

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

ofList :: Foldable t => t T -> T
ofList = Std.foldr Cons Nil

addMetaToCar :: Atom -> T -> T
addMetaToCar (A _ lineInfo) (Cons (Atom (A term _)) xs) =
  Cons (Atom (A term lineInfo)) xs
addMetaToCar _ xs = xs

car :: T -> T
car (Cons x _) = x
car Nil = Nil
car (Atom a) = Atom a

showNoParens :: T -> String
showNoParens (Cons car cdr)
  | showNoParens cdr == ")" =
    show car <> showNoParens cdr
  | otherwise =
    show car <> " " <> showNoParens cdr
showNoParens xs = show xs

-- TODO ∷ make reader instance

instance Show T where
  show (Cons car cdr)
    | show cdr == ")" =
      "(" <> show car <> showNoParens cdr
    | otherwise =
      "(" <> show car <> " " <> showNoParens cdr
  show (Atom (A x _)) =
    show (NameSymbol.toSymbol x)
  show Nil = ")"
