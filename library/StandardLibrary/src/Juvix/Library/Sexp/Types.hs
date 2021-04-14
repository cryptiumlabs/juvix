{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Juvix.Library.Sexp.Types where

import Control.Lens hiding (List, (:>), (|>))
import Juvix.Library hiding (foldr, show, toList)
import qualified Juvix.Library.LineNum as LineNum
import qualified Juvix.Library.NameSymbol as NameSymbol
import Prelude (Show (..), String)

-- Can't use length here for sad reasons
{-@ measure lengthM @-}
{-@ lengthM :: T -> Nat @-}
lengthM :: T -> Int
lengthM Atom {} = 0
lengthM Nil = 0
lengthM (Cons _ xs) = 1 + lengthM xs

{-@ data T [lengthM] @-}

-- TODO ∷ make Atom generic, and have it conform to an interfac?
-- This way we can erase information later!
data T
  = Atom Atom
  | Cons {tCar :: T, tCdr :: T}
  | Nil
  deriving (Eq)

data Atom
  = A {atomName :: NameSymbol.T, atomLineNum :: Maybe LineNum.T}
  | N {atomNum :: Integer, atomLineNum :: Maybe LineNum.T}
  deriving (Show)

instance Eq Atom where
  A n1 _ == A n2 _ = n1 == n2
  N i1 _ == N i2 _ = i1 == i2
  _ == _ = False

instance Ord Atom where
  compare (A n1 _) (A n2 _) = compare n1 n2
  compare (N i1 _) (N i2 _) = compare i1 i2
  compare (N _ _) (A _ _) = GT
  compare (A _ _) (N _ _) = LT

makeLensesWith camelCaseFields ''Atom

toList :: T -> Maybe [T]
toList (Cons x xs) = (x :) <$> toList xs
toList Nil = pure []
toList (Atom _a) = Nothing

infixr 5 :>

pattern (:>) :: T -> T -> T
pattern x :> xs = Cons x xs

{-# COMPLETE (:>), Atom, Nil #-}

pattern List :: [T] -> T
pattern List xs <- (toList -> Just xs)

{-# COMPLETE List, Atom #-}

-- TODO ∷ make reader instance

instance Show T where
  show (Cons car cdr)
    | take 1 (showNoParens cdr) == ")" =
      "(" <> show car <> showNoParens cdr
    | otherwise =
      "(" <> show car <> " " <> showNoParens cdr
  show (Atom (A x _)) =
    show (NameSymbol.toSymbol x)
  show (Atom (N x _)) =
    show x
  show Nil = "()"

showNoParens :: T -> String
showNoParens (Cons car cdr)
  | showNoParens cdr == ")" =
    show car <> showNoParens cdr
  | otherwise =
    show car <> " " <> showNoParens cdr
showNoParens Nil = ")"
showNoParens xs = show xs
