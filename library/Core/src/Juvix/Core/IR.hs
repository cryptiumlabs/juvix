module Juvix.Core.IR
  ( module Juvix.Core.IR,
    module IR,
  )
where

import Juvix.Core.IR.Evaluator as IR
import Juvix.Core.IR.Typechecker as IR
  ( Annotation,
    Annotation' (..),
    AnnotationT,
    BindAnnotation,
    BindAnnotation' (..),
    BindAnnotationT,
    Context,
    EnvCtx,
    EnvTypecheck' (..),
    EnvTypecheck,
    Leftovers (..),
    TypecheckError,
    TypecheckError' (..),
    ValueT,
    GlobalsT,
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
    evalTC,
  )
import qualified Juvix.Core.IR.Typechecker as TC
import Juvix.Core.IR.Types as IR
import Juvix.Library

type TermT primTy primVal = TC.Term primTy primVal
type ElimT primTy primVal = TC.Elim primTy primVal

execTC ::
  GlobalsT primTy primVal ->
  EnvTypecheck primTy primVal a ->
  (Either (TypecheckError primTy primVal) a, EnvCtx primTy primVal)
execTC = TC.exec
