{-# LANGUAGE LiberalTypeSynonyms #-}

module Contextualise.Contextify where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Juvix.Core.Common.Context as Context
import qualified Juvix.Frontend.Parser as Parser
import qualified Juvix.Frontend.Types as AST
import qualified Juvix.FrontendContextualise as Contextualize
import qualified Juvix.FrontendDesugar as Desugar
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

--------------------------------------------------------------------------------
-- Top
--------------------------------------------------------------------------------

top :: T.TestTree
top =
  T.testGroup
    "contextify tests:"
    [infixPlaceTest]

--------------------------------------------------------------------------------
-- tests
--------------------------------------------------------------------------------

infixPlaceTest :: T.TestTree
infixPlaceTest =
  ctx Context.!? (NameSymbol.fromSymbol "+")
    |> fmap (Context.precedence . Context.extractValue)
    |> (T.@=? Just (Context.Pred Context.Left 5))
    |> T.testCase
      "infix properly adds precedence"
  where
    Right (ctx, _) =
      Contextualize.contextify ((NameSymbol.fromSymbol "Foo", deusgared) :| [])
    Right deusgared =
      Desugar.op . AST.extractTopLevel
        <$> Parser.parseOnly "let (+) = 3 declare infixl (+) 5"
