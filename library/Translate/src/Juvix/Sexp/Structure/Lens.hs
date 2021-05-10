{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
module Juvix.Sexp.Structure.Lens where

import qualified Control.Lens as Lens hiding ((|>))
import Juvix.Sexp.Structure

Lens.makeLensesWith Lens.camelCaseFields ''Defun
Lens.makeLensesWith Lens.camelCaseFields ''DefunMatch
Lens.makeLensesWith Lens.camelCaseFields ''ArgBody
Lens.makeLensesWith Lens.camelCaseFields ''Signature
Lens.makeLensesWith Lens.camelCaseFields ''DefunSigMatch
Lens.makeLensesWith Lens.camelCaseFields ''Let
Lens.makeLensesWith Lens.camelCaseFields ''LetMatch
Lens.makeLensesWith Lens.camelCaseFields ''Cond
Lens.makeLensesWith Lens.camelCaseFields ''PredAns
Lens.makeLensesWith Lens.camelCaseFields ''If
Lens.makeLensesWith Lens.camelCaseFields ''IfNoElse
Lens.makeLensesWith Lens.camelCaseFields ''Case
Lens.makeLensesWith Lens.camelCaseFields ''DeconBody
Lens.makeLensesWith Lens.camelCaseFields ''Do
Lens.makeLensesWith Lens.camelCaseFields ''Arrow
Lens.makeLensesWith Lens.camelCaseFields ''Lambda
Lens.makeLensesWith Lens.camelCaseFields ''Record
Lens.makeLensesWith Lens.camelCaseFields ''Punned
Lens.makeLensesWith Lens.camelCaseFields ''NotPunned
Lens.makeLensesWith Lens.camelCaseFields ''RecordNoPunned
