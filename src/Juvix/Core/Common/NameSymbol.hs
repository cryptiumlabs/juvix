module Juvix.Core.Common.NameSymbol where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import Juvix.Library
import qualified Prelude (foldr1)

type T = NonEmpty Symbol

toSymbol :: T -> Symbol
toSymbol =
  Prelude.foldr1 (\x acc -> x <> "." <> acc)

fromSymbol :: Symbol -> T
fromSymbol =
  NonEmpty.fromList . fmap internText . Text.splitOn "." . textify

prefixOf :: T -> T -> Bool
prefixOf smaller larger =
  case takePrefixOfInternal smaller larger of
    Just _ -> True
    Nothing -> False

takePrefixOf :: T -> T -> Maybe T
takePrefixOf smaller larger =
  case takePrefixOfInternal smaller larger of
    Just [] -> Nothing
    Nothing -> Nothing
    Just (x : xs) -> Just (x :| xs)

takePrefixOfInternal :: T -> T -> Maybe [Symbol]
takePrefixOfInternal (s :| smaller) (b :| bigger)
  | b == s = recurse smaller bigger
  | otherwise = Nothing
  where
    recurse [] ys = Just ys
    recurse _ [] = Nothing
    recurse (x : xs) (y : ys)
      | x == y = recurse xs ys
      | otherwise = Nothing

cons :: Symbol -> T -> T
cons = NonEmpty.cons

hd :: T -> Symbol
hd = NonEmpty.head
