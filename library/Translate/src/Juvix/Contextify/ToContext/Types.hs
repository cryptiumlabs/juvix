{-# LANGUAGE LiberalTypeSynonyms #-}

module Juvix.Contextify.ToContext.Types where

import qualified Juvix.Core.Common.Context as Context
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.Sexp as Sexp

type ContextSexp =
  Context.T Sexp.T Sexp.T Sexp.T

type DefinitionSexp =
  Context.Definition Sexp.T Sexp.T Sexp.T

data PassSexp = PS
  { ctxS :: ContextSexp,
    opensS :: [NameSymbol.T],
    modsDefinedS :: [NameSymbol.T]
  }
  deriving (Show)
