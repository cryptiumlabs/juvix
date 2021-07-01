{-# LANGUAGE LiberalTypeSynonyms #-}

module Juvix.Core
  ( module Juvix.Core.Erased.Algorithm,
    module Juvix.Core.Translate,
    module Juvix.Core.Pipeline,
    module Juvix.Core.Types,
    module Juvix.Core,
  )
where

import qualified Juvix.Context as Context
import qualified Juvix.Contextify as Contextify
import Juvix.Core.Erased.Algorithm (erase, eraseAnn)
import qualified Juvix.Core.HR.Pretty as HR
import Juvix.Core.Pipeline
import Juvix.Core.Translate
import Juvix.Core.Types
import qualified Juvix.Frontend.Types as Initial
import qualified Juvix.FrontendDesugar as Desugar
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.PrettyPrint as PP
import qualified Juvix.Sexp as Sexp

data Error
  = ContextErr Contextify.ResolveErr
  | NoInput
  deriving (Show)

type instance PP.Ann Error = ()

instance PP.PrettyText Error where
  prettyT = \case
    ContextErr err -> PP.show err -- FIXME
    NoInput -> PP.text "no input"

-- TODO ∷ update the target when the last pass is finished,
-- that way we can get the T out
ofFrontend ::
  [(NameSymbol.T, [Initial.TopLevel])] ->
  IO (Either Error (Context.T Sexp.T Sexp.T Sexp.T))
ofFrontend syn =
  case fmap (second Desugar.op) syn of
    [] ->
      pure $ Left NoInput
    x : xs -> do
      contextd <- Contextify.op (x :| xs)
      pure $ case contextd of
        Left errr -> Left (ContextErr errr)
        Right con -> Right con
