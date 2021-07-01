module Juvix.ToCore.FromFrontend.Transform.Usage (transformUsage, transformGUsage) where

import qualified Juvix.Core.Base as Core
import qualified Juvix.Core.IR as IR
import qualified Juvix.Core.Base as Core
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.Usage as Usage
import qualified Juvix.Sexp as Sexp
import Juvix.ToCore.FromFrontend.Transform.Helpers
  ( isOmega,
  )
import Juvix.ToCore.Types
  ( Error (..),
    HasCoreSigs,
    HasThrowFF,
    throwFF,
  )

-- | Retrieve usage from numeric atom
transformUsage ::
  ( Show primTy,
    Show primVal,
    HasThrowFF primTy primVal m,
    HasCoreSigs primTy primVal m
  ) =>
  NameSymbol.Mod ->
  Sexp.T ->
  m Usage.T
transformUsage _ (Sexp.Atom Sexp.N {atomNum = i}) | i >= 0 = pure $ Usage.SNat $ fromInteger i
transformUsage q e = do
  o <- isOmega q e
  if o then pure Usage.Omega else throwFF $ NotAUsage e

transformGUsage ::
  ( Show primTy,
    Show primVal,
    HasThrowFF primTy primVal m,
    HasCoreSigs primTy primVal m
  ) =>
  NameSymbol.Mod ->
  Maybe Sexp.T ->
  m Core.GlobalUsage
transformGUsage _ Nothing = pure Core.GOmega
transformGUsage _ (Just (Sexp.Atom Sexp.N {atomNum = 0})) = pure Core.GZero
transformGUsage q (Just e) = do
  o <- isOmega q e
  if o then pure Core.GOmega else throwFF $ NotAGUsage e
