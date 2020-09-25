{-# LANGUAGE LiberalTypeSynonyms #-}

module FrontendContextualise.Contextify where

import qualified Juvix.FrontendContextualise as Contextualize
import qualified Juvix.Frontend.Parser as Parser
import qualified Juvix.FrontendDesugar as Desugar
import qualified Data.List.NonEmpty as NonEmpty

import Juvix.Library


test = Contextualize.contextify ((("Foo" :| []), foo) :| [])
  where
    Right foo = Desugar.f <$> (Parser.parseOnly "let (+) = 3 infixl (+) 5")
