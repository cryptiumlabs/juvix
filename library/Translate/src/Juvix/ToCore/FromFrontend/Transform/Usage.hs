module Juvix.ToCore.FromFrontend.Transform.Usage where

import qualified Juvix.Core.IR as IR
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.Sexp as Sexp
import qualified Juvix.Library.Usage as Usage
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
  m IR.GlobalUsage
transformGUsage _ Nothing = pure IR.GOmega
transformGUsage _ (Just (Sexp.Atom Sexp.N {atomNum = 0})) = pure IR.GZero
transformGUsage q (Just e) = do
  o <- isOmega q e
  if o then pure IR.GOmega else throwFF $ NotAGUsage e
