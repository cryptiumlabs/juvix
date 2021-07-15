{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Juvix.Sexp.Structure.Lens where

import qualified Control.Lens as Lens hiding ((|>))
import qualified Juvix.Sexp.Structure.CoreNamed as Named
import qualified Juvix.Sexp.Structure.Frontend as Frontend
import qualified Juvix.Sexp.Structure.Transition as Transition

Lens.makeLensesWith Lens.camelCaseFields ''Transition.DefunMatch
Lens.makeLensesWith Lens.camelCaseFields ''Transition.ArgBody
Lens.makeLensesWith Lens.camelCaseFields ''Frontend.Signature
Lens.makeLensesWith Lens.camelCaseFields ''Transition.DefunSigMatch
Lens.makeLensesWith Lens.camelCaseFields ''Transition.RecordNoPunned
Lens.makeLensesWith Lens.camelCaseFields ''Transition.LetHandler
Lens.makeLensesWith Lens.camelCaseFields ''Frontend.Defun
Lens.makeLensesWith Lens.camelCaseFields ''Frontend.Let
Lens.makeLensesWith Lens.camelCaseFields ''Transition.LetMatch
Lens.makeLensesWith Lens.camelCaseFields ''Frontend.Cond
Lens.makeLensesWith Lens.camelCaseFields ''Frontend.PredAns
Lens.makeLensesWith Lens.camelCaseFields ''Transition.If
Lens.makeLensesWith Lens.camelCaseFields ''Transition.IfNoElse
Lens.makeLensesWith Lens.camelCaseFields ''Frontend.Case
Lens.makeLensesWith Lens.camelCaseFields ''Frontend.DeconBody
Lens.makeLensesWith Lens.camelCaseFields ''Frontend.Do
Lens.makeLensesWith Lens.camelCaseFields ''Frontend.Arrow
Lens.makeLensesWith Lens.camelCaseFields ''Frontend.Lambda
Lens.makeLensesWith Lens.camelCaseFields ''Frontend.Record
Lens.makeLensesWith Lens.camelCaseFields ''Frontend.Punned
Lens.makeLensesWith Lens.camelCaseFields ''Frontend.NotPunned
Lens.makeLensesWith Lens.camelCaseFields ''Frontend.Infix
Lens.makeLensesWith Lens.camelCaseFields ''Frontend.OpenIn
Lens.makeLensesWith Lens.camelCaseFields ''Frontend.Open
Lens.makeLensesWith Lens.camelCaseFields ''Frontend.Declare
Lens.makeLensesWith Lens.camelCaseFields ''Frontend.Declaim
Lens.makeLensesWith Lens.camelCaseFields ''Frontend.Type
Lens.makeLensesWith Lens.camelCaseFields ''Frontend.LetType
Lens.makeLensesWith Lens.camelCaseFields ''Frontend.LetSignature
Lens.makeLensesWith Lens.camelCaseFields ''Frontend.DefModule
Lens.makeLensesWith Lens.camelCaseFields ''Frontend.LetModule
Lens.makeLensesWith Lens.camelCaseFields ''Frontend.DefHandler
Lens.makeLensesWith Lens.camelCaseFields ''Frontend.Effect
Lens.makeLensesWith Lens.camelCaseFields ''Named.Star
Lens.makeLensesWith Lens.camelCaseFields ''Named.PrimTy
Lens.makeLensesWith Lens.camelCaseFields ''Named.Prim
Lens.makeLensesWith Lens.camelCaseFields ''Named.Pi
Lens.makeLensesWith Lens.camelCaseFields ''Named.Binder
Lens.makeLensesWith Lens.camelCaseFields ''Named.Lam
Lens.makeLensesWith Lens.camelCaseFields ''Named.Sigma
Lens.makeLensesWith Lens.camelCaseFields ''Named.Pair
Lens.makeLensesWith Lens.camelCaseFields ''Named.Let
Lens.makeLensesWith Lens.camelCaseFields ''Named.Var
Lens.makeLensesWith Lens.camelCaseFields ''Named.App
Lens.makeLensesWith Lens.camelCaseFields ''Named.Ann
Lens.makeLensesWith Lens.camelCaseFields ''Named.Meta
Lens.makeLensesWith Lens.camelCaseFields ''Named.Dot
