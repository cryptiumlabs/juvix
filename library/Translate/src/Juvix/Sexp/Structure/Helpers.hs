module Juvix.Sexp.Structure.Helpers (fromGen, toStarList, fromStarList) where

import Juvix.Library
import qualified Juvix.Library.Sexp as Sexp

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
