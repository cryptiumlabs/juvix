module Interfaces

import Basics

-- For now...
import public Tezos.Prim

%access public export
%default total

not : Bool -> Bool
not True  = False
not False = True

infix 6 ==, /=, <, <=, >, >=
infixl 7 <<, >>
infixl 8 +, -
infixl 9 *, /

infixl 9 `div`, `mod`

interface Eq ty where
  (==) : ty -> ty -> Bool
  (/=) : ty -> ty -> Bool

  x /= y = not (x == y)
  x == y = not (x /= y)

interface Num ty where
  (+) : ty -> ty -> ty
  (*) : ty -> ty -> ty

  fromInteger : Integer -> ty

interface Num ty => Neg ty where
  negate : ty -> ty
  (-) : ty -> ty -> ty

interface Num ty => Abs ty where
  abs : ty -> ty

interface Num ty => Fractional ty where
  (/) : ty -> ty -> ty

  recip : ty -> ty
  recip x = 1 / x

interface Num ty => Integral ty where
  div : ty -> ty -> ty
  mod : ty -> ty -> ty

data Ordering = LT | EQ | GT

interface Eq ty => Ord ty where
  (<) : ty -> ty -> Bool
  (>) : ty -> ty -> Bool

  {-
  compare : ty -> ty -> Ordering

  (<) : ty -> ty -> Bool
  (<) x y with (compare x y)
      (<) x y | LT = True
      (<) x y | _  = False

  (>) : ty -> ty -> Bool
  (>) x y with (compare x y)
      (>) x y | GT = True
      (>) x y | _  = False

  (<=) : ty -> ty -> Bool
  (<=) x y = x < y || x == y

  (>=) : ty -> ty -> Bool
  (>=) x y = x > y || x == y

  max : ty -> ty -> ty
  max x y = if x > y then x else y

  min : ty -> ty -> ty
  min x y = if (x < y) then x else y
  -}

infixl 6 <+>

interface Semigroup ty where
  (<+>) : ty -> ty -> ty

interface Semigroup ty => Monoid ty where
  neutral : ty
