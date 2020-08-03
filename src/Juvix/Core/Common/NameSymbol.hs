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
