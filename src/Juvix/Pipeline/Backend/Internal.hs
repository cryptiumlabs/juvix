{-# LANGUAGE TypeFamilyDependencies #-}

module Juvix.Pipeline.Backend.Internal
  ( HasBackend (..),
  )
where

import qualified Juvix.Core.Application as CoreApp
import qualified Juvix.Core.Common.Context as Context
import qualified Juvix.Core.ErasedAnn as ErasedAnn
import Juvix.Library
import qualified Juvix.Library.Sexp as Sexp
import Juvix.Pipeline.Compile

class HasBackend b where
  type Ty b = ty | ty -> b
  type Val b = val | val -> b

  typecheck ::
    Context.T Sexp.T Sexp.T Sexp.T ->
    Pipeline (ErasedAnn.AnnTerm (Ty b) (ErasedAnn.TypedPrim (Ty b) (Val b)))
  compile ::
    ErasedAnn.AnnTerm (Ty b) (ErasedAnn.TypedPrim (Ty b) (Val b)) ->
    Pipeline Text
