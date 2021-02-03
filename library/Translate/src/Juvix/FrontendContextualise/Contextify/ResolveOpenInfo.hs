module Juvix.FrontendContextualise.Contextify.ResolveOpenInfo where

import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol


data PreQualified
  = Pre
      { opens :: [NameSymbol.T],
        implicitInner :: [NameSymbol.T],
        explicitModule :: NameSymbol.T
      }
  deriving (Show, Eq)


