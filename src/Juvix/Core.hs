{-# LANGUAGE LiberalTypeSynonyms #-}

module Juvix.Core
  ( module Juvix.Core.Erasure,
    module Juvix.Core.Translate,
    module Juvix.Core.Pipeline,
    module Juvix.Core.Types,
    module Juvix.Core,
  )
where

import Juvix.Core.Erasure (erase, eraseAnn)
import Juvix.Core.Pipeline
import Juvix.Core.Translate
import Juvix.Core.Types
import qualified Juvix.Frontend.Types as Initial
import qualified Juvix.FrontendContextualise as Contextualise
import qualified Juvix.FrontendContextualise.InfixPrecedence.Environment as Target
import qualified Juvix.FrontendDesugar as Desugar
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol

data Error
  = ContextErr Contextualise.Error
  | NoInput
  deriving (Show)

-- TODO ∷ update the target when the last pass is finished,
-- that way we can get the T out
ofFrontend ::
  [(NameSymbol.T, [Initial.TopLevel])] -> IO (Either Error Target.FinalContext)
ofFrontend syn =
  case fmap (second Desugar.op) syn of
    [] ->
      pure $ Left NoInput
    x : xs -> do
      contextd <- Contextualise.op (x :| xs)
      pure $ case contextd of
        Left errr -> Left (ContextErr errr)
        Right con -> Right con
