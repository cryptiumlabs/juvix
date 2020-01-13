{-# LANGUAGE TypeFamilies #-}

module Juvix.Core.IRAnn.Types where

import qualified Juvix.Core.IR.Types as IR
import Juvix.Core.Usage

data Typed

instance IR.TEExt Typed where
  type XLam Typed primTy primVal = TermAnnotation primTy primVal
  type XElim Typed primTy primVal = TermAnnotation primTy primVal
  type XApp Typed primTy primVal =
      (TermAnnotation primTy primVal, TermAnnotation primTy primVal)

type TermAnnotation primTy primVal = (Usage, Term primTy primVal)

type Term = IR.Term' Typed
type Elim = IR.Elim' Typed
