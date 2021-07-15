module Juvix.Sexp.Structure.Helpers
  ( fromGen,
    toStarList,
    fromStarList,
    toNameSymbol,
    fromNameSymbol,
    fromInteger,
    toInteger,
  )
where

import Juvix.Library hiding (fromInteger, toInteger)
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Sexp as Sexp

fromGen :: (t -> Bool) -> (t -> Maybe a) -> t -> Maybe a
fromGen pred func form
  | pred form = func form
  | otherwise = Nothing

toStarList :: (t -> Sexp.T) -> [t] -> Sexp.T
toStarList f (x : xs) =
  f x Sexp.:> toStarList f xs
toStarList _ [] = Sexp.Nil

fromStarList :: (Sexp.T -> Maybe a) -> Sexp.T -> Maybe [a]
fromStarList f (x Sexp.:> xs) =
  (:) <$> f x <*> fromStarList f xs
fromStarList _ Sexp.Nil = Just []
fromStarList _ _ = Nothing

toNameSymbol :: Sexp.T -> Maybe NameSymbol.T
toNameSymbol = Sexp.nameFromT

fromNameSymbol :: NameSymbol.T -> Sexp.T
fromNameSymbol = Sexp.atom

fromInteger :: Integer -> Sexp.T
fromInteger = Sexp.number

toInteger :: Sexp.T -> Maybe Integer
toInteger (Sexp.Atom (Sexp.N num _)) = Just num
toInteger Sexp.Nil = Nothing
toInteger Sexp.Cons {} = Nothing
toInteger (Sexp.Atom Sexp.A {}) = Nothing
