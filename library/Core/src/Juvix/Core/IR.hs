module Juvix.Core.IR
  ( module Juvix.Core.IR,
    module IR,
  )
where

import Juvix.Core.IR.CheckTerm as IR
  ( Leftovers (..),
    evalTC,
    leftoverOk,
    leftoversOk,
    typeElim,
    typeElimWith,
    typeTerm,
    typeTermWith,
  )
import Juvix.Core.IR.Evaluator as IR hiding
  ( Error (..),
  )
import Juvix.Core.IR.Typechecker as IR
  ( Annotation,
    Annotation' (..),
    AnnotationT,
    BindAnnotation,
    BindAnnotation' (..),
    BindAnnotationT,
    Context,
    EnvCtx,
    EnvTypecheck,
    EnvTypecheck' (..),
    GlobalsT,
    TypecheckError,
    TypecheckError' (..),
    UContext,
    ValueT,
    getElimAnn,
    getTermAnn,
    lookupCtx,
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
