{-# LANGUAGE LiberalTypeSynonyms #-}

module Juvix.Core
  ( module Juvix.Core.Erasure,
    module Juvix.Core.Translate,
    module Juvix.Core.Pipeline,
    module Juvix.Core.Usage,
    module Juvix.Core.Types,
    module Juvix.Core,
  )
where

import qualified Juvix.Core.Common.NameSymbol as NameSymbol
import Juvix.Core.Erasure (erase, eraseAnn)
import Juvix.Core.Pipeline
import Juvix.Core.Translate
import Juvix.Core.Types
import Juvix.Core.Usage
import qualified Juvix.Frontend.Types as Initial
import qualified Juvix.FrontendContextualise as Contextualise
import qualified Juvix.FrontendContextualise.InfixPrecedence.Environment as Target
import qualified Juvix.FrontendDesugar as Desugar
import qualified Juvix.Frontend.Parser as Parser
import Juvix.Library

data Error
  = ContextErr Contextualise.Error
  | NoInput
  deriving (Show)

-- TODO ∷ update the target when the last pass is finished,
-- that way we can get the T out
ofFrontend ::
  [(NameSymbol.T, [Initial.TopLevel])] -> Either Error Target.FinalContext
ofFrontend syn =
  let newSyn = fmap (\(name, syntax) -> (name, Desugar.f syntax)) syn
   in case newSyn of
        [] ->
          Left NoInput
        x : xs ->
          case Contextualise.f (x :| xs) of
            Left errr -> Left (ContextErr errr)
            Right con -> Right con
