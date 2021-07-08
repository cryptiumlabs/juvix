{-# LANGUAGE LiberalTypeSynonyms #-}

module Juvix.Pipeline.Frontend
  ( frontendToSexp,
    Error (..),
  )
where

import qualified Juvix.Context as Context
import qualified Juvix.Contextify as Contextify
import Juvix.Core.Erased.Algorithm (erase, eraseAnn)
import qualified Juvix.Core.HR.Pretty as HR
import Juvix.Core.Translate
import Juvix.Core.Types
import qualified Juvix.Frontend.Types as Initial
import qualified Juvix.FrontendDesugar as Desugar
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.PrettyPrint as PP
import qualified Juvix.Sexp as Sexp

-- | Frontend Error
data Error
  = ContextErr Contextify.ResolveErr
  | DesugarErr
  deriving (Show)

type instance PP.Ann Error = ()

instance PP.PrettyText Error where
  prettyT = \case
    ContextErr err -> PP.show err -- FIXME
    DesugarErr -> PP.text "no input after desugaring"

-- TODO ∷ update the target when the last pass is finished,
-- that way we can get the T out
frontendToSexp ::
  [(NameSymbol.T, [Initial.TopLevel])] ->
  IO (Either Error (Context.T Sexp.T Sexp.T Sexp.T))
frontendToSexp syn =
  case fmap (second Desugar.op) syn of
    [] ->
      pure $ Left DesugarErr
    x : xs -> do
      contextd <- Contextify.op (x :| xs)
      pure $ case contextd of
        Left errr -> Left (ContextErr errr)
        Right con -> Right con
