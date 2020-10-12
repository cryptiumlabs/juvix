module Juvix.Core.IR
  ( module Juvix.Core.IR,
    module IR,
  )
where

import Juvix.Core.IR.Evaluator as IR
import Juvix.Core.IR.Typechecker as IR
  ( Annotation,
    Annotation' (..),
    BindAnnotation,
    BindAnnotation' (..),
    Context,
    EnvCtx,
    EnvTypecheck (..),
    Leftovers (..),
    TypecheckError,
    TypecheckError' (..),
    UContext,
    getElimAnn,
    getTermAnn,
    leftoverOk,
    leftoversOk,
    lookupCtx,
    typeElim,
    typeElimWith,
    typeTerm,
    typeTermWith,
  )
import qualified Juvix.Core.IR.Typechecker as TC
import Juvix.Core.IR.Types as IR
import Juvix.Library

execTC ::
  Globals primTy primVal ->
  EnvTypecheck primTy primVal a ->
  (Either (TypecheckError primTy primVal) a, EnvCtx primTy primVal)
execTC = TC.exec
