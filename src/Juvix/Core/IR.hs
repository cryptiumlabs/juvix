module Juvix.Core.IR
  ( module Juvix.Core.IR,
    module IR,
  )
  where

import Juvix.Core.IR.Evaluator as IR
import Juvix.Core.IR.Typechecker as IR
  ( Leftovers (..),
    leftoversOk, leftoverOk,
    typeTerm, typeTermWith,
    typeElim, typeElimWith,
    EnvTypecheck (..),
    EnvCtx,

    -- ….Types
    Annotation' (..),
    Annotation,
    BindAnnotation' (..),
    BindAnnotation,
    TypecheckError' (..),
    TypecheckError,
    getTermAnn,
    getElimAnn,

    -- * reexports from ….Env
    Globals,
    Global (..),
    Context,
    UContext,
    lookupCtx,
  )
import Juvix.Core.IR.Types as IR

import Juvix.Library
import qualified Juvix.Core.IR.Typechecker as TC

execTC ::
  Globals primTy primVal ->
  EnvTypecheck primTy primVal a ->
  (Either (TypecheckError primTy primVal) a, EnvCtx primTy primVal)
execTC = TC.exec
