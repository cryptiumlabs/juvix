module Juvix.Core.Common.NameSymbol where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import Juvix.Library

type T = NonEmpty Symbol

toSymbol :: T -> Symbol
toSymbol =
  intern . foldr (\x acc -> unintern x <> "." <> acc) mempty

fromSymbol :: Symbol -> T
fromSymbol =
  NonEmpty.fromList . fmap internText . Text.splitOn "." . textify

subsetOf :: T -> T -> Bool
subsetOf smaller larger =
  case takeSubSetOfInternal smaller larger of
    Just _ -> True
    Nothing -> False

takeSubSetOf :: T -> T -> Maybe T
takeSubSetOf smaller larger =
  case takeSubSetOfInternal smaller larger of
    Just [] -> Nothing
    Nothing -> Nothing
    Just (x : xs) -> Just (x :| xs)

takeSubSetOfInternal :: T -> T -> Maybe [Symbol]
takeSubSetOfInternal  (s :| smaller) (b :| bigger)
  | b == s = recurse smaller bigger
  | otherwise = Nothing
  where
    recurse [] ys = Just ys
    recurse _ [] = Nothing
    recurse (x : xs) (y : ys)
      | x == y = recurse xs ys
      | otherwise = Nothing
